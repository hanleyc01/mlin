//! The `mir` is the Rust runtime representation of [MIR](https://github.com/vnmakarov/mir), which,
//! through [`bindgen`], is able to be then consumed through the MIR C-API.

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::array;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::marker::PhantomData;
use std::os::fd::AsRawFd;
use std::os::unix::io::IntoRawFd;

use libc::FILE as LibcFile;
use tinyvec::{ArrayVec, TinyVec};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

/// Wrapper around [`MIR_context_t`]. Represents the implicit state of the program.
/// The [`Context`] is responsible for the creation of new [`Module`]'s, [`Function`]'s,
/// and even [`Insn`]'s.
#[derive(Debug, Clone)]
pub struct Context<'ctx> {
    marker: PhantomData<&'ctx Context<'ctx>>,
    ctx: *mut MIR_context,
}

impl<'ctx> Context<'ctx> {
    /// Initializes an [`Context`] by calling [`_MIR_init`], returning back a
    /// `*mut MIR_context` (or [`MIR_context_t`]).
    pub fn init() -> Self {
        let ctx = unsafe { _MIR_init() };
        let marker = PhantomData;

        Self { ctx, marker }
    }

    /// Outputs `MIR` textual representation of the program into a [`std::fs::File`]
    pub fn output<'a>(&'a self, f: File) {
        unsafe {
            let c_file: *mut FILE = fdopen(
                f.into_raw_fd(),
                CStr::from_bytes_with_nul(b"r\0")
                    .expect("Unable to convert file string to CSing!")
                    .as_ptr(),
            );

            MIR_output(self.ctx, c_file)
        }
    }

    /// Read textual MIR representation as given by a string.
    pub fn scan_string<'a>(&'a self, src: &str) {
        unsafe {
            let s = CString::new(src).expect("Unable to create CSing in `scan_string`!");
            let s_ptr = s.as_ptr();
            MIR_scan_string(self.ctx, s_ptr)
        }
    }

    /// Read a **binary MIR representation** from a given file.
    pub fn read<'a>(&'a self, f: File) {
        unsafe {
            // convert the `File` into an acceptable C equivalent
            let c_file: *mut FILE = fdopen(
                f.into_raw_fd(),
                CStr::from_bytes_with_nul(b"r\0")
                    .expect("Unable to convert file string to CSing!")
                    .as_ptr(),
            );

            MIR_read(self.ctx, c_file)
        }
    }

    /// Write to a **binary MIR representation** file.
    pub fn write<'a>(&'a self, mut f: File) {
        unsafe {
            // convert the `File` into an acceptable C equivalent
            let c_file: *mut FILE = fdopen(
                f.into_raw_fd(),
                CStr::from_bytes_with_nul_unchecked(b"w\0").as_ptr(),
            );

            MIR_output(self.ctx, c_file);
        }
    }

    pub fn write_out<'a>(&'a self) {
        unsafe {
            let c_file: *mut FILE = fdopen(
                libc::STDOUT_FILENO.as_raw_fd(),
                CStr::from_bytes_with_nul_unchecked(b"w\0").as_ptr(),
            );
            MIR_output(self.ctx, c_file);
        }
    }

    /// Create a new [`Module`].
    pub fn new_module<'a, S: AsRef<str>>(&'a self, name: S) -> Module<'a> {
        unsafe {
            let c_name = CString::new(name.as_ref()).unwrap();
            let c_name_ptr = c_name.into_raw();
            let mut module = MIR_new_module(self.ctx, c_name_ptr);

            Module {
                ctx: self,
                ptr: module,
                marker: PhantomData,
            }
        }
    }

    /// Finish a module, consuming its reference. **NOTE**: this does not free the memory of the module itself!
    /// Rather, it *consumes* the reference, which eliminates any further possible aliasing or mutation. To free the
    /// memory of the [`Module`], see [`Context::finish`].
    pub fn finish_module<'a>(&'a self, module: Module<'a>) {
        unsafe {
            MIR_finish_module(self.ctx);
        }
    }

    /// Unsafe consumption of the reference a [`Context`], freeing its memory.
    /// Note that this **must** be called if you wish for the memory to be freed,
    /// otherwise you will have a leak! (This is the result of the C-API of MIR :()
    pub unsafe fn finish(mut self) {
        MIR_finish(self.ctx)
    }
}

impl Drop for Context<'_> {
    fn drop(&'_ mut self) {
        unsafe { MIR_finish(self.ctx) }
    }
}

/// Thin wrapper around `*mut MIR_module`
pub struct Module<'ctx> {
    ctx: &'ctx Context<'ctx>,
    marker: PhantomData<&'ctx Module<'ctx>>,
    ptr: *mut MIR_module,
}

impl<'ctx> Module<'ctx> {
    /// Create a new reference to a function; note that one can only create and change the state
    /// of **one function at a time**. It is unsafe to do otherwise!
    pub fn new_function<S: AsRef<str>>(
        &'ctx self,
        name: S,
        res_types: &[Data],
        mut args: &[Var<'ctx>],
    ) -> Function<'ctx> {
        unsafe {
            // making a call to [`MIR_new_func_arr`]
            let c_name = CString::new(name.as_ref()).unwrap();
            let c_name_ptr = c_name.into_raw();

            // BEGIN TODO:
            // Also determine if these references actually live long enough? I'm pretty sure
            // that `MIR_new_func_arr` allocates new vectors for these things, but we'll see!

            // TODO: figure out if the use of `TinyVec` is actually an optimization
            let nres = res_types.len();
            let mut res_types = res_types.iter().map(|x| Into::<u32>::into(x));
            let mut res_types_arr: TinyVec<[u32; 6]> = TinyVec::from_iter(res_types);
            let res_types_ptr = res_types_arr.as_mut_ptr();

            let nargs: usize = args.len();
            let vars = (*args).as_ptr() as *mut MIR_var_t;

            let mut func =
                MIR_new_func_arr(self.ctx.ctx, c_name_ptr, nres, res_types_ptr, nargs, vars);

            Function {
                module: self,
                marker: PhantomData,
                ptr: func,
            }
        }
    }

    /// Unsafe consumption of the reference to a [`Function`]. Similar to [`Context::finish`],
    /// this **does not free** the memory of the function, only the reference to it. If we wish to
    /// free the memory, see the above-mentioned [`Context::finish`].
    pub fn finish_function<'a>(&'a self, function: Function<'a>) {
        unsafe { MIR_finish_func(self.ctx.ctx) }
    }
}

/// Thin wrapper around `MIR_item_t`, specifically, functions.
/// [`Function`]'s are [`Module`] items.
/// + They have a **frame**, which is a stack memory reserved for each function
/// invocation.
/// + They have **local variables**, or **registers**, which are a part of **arguments**,
/// + [`Function`]'s can have a possible number of results, and the combination of their
/// types is machine-defined. E.g., x86-64 copmuters can have up to six results, and return
/// two integer values.
/// + One can only create on [`Function`] at a time.
pub struct Function<'module> {
    module: &'module Module<'module>,
    marker: PhantomData<&'module Function<'module>>,
    ptr: *mut MIR_item,
}

impl<'module> Function<'module> {
    /// Creates a new non-argument function variable. THe only permitted integer type for the register
    /// is `MIRData::I64`. Names of the form `t<number>` cannot be used as they are reserved for internal
    /// purposes.
    pub fn func_reg<S: AsRef<str>>(&self, data: Data, s: S) -> Reg {
        unsafe {
            let c_name = CString::new(s.as_ref()).unwrap();
            let c_name_ptr = c_name.into_raw();
            let type_ = data.into();
            MIR_new_func_reg(self.module.ctx.ctx, (*self.ptr).u.func, type_, c_name_ptr)
        }
    }

    /// Creates an argument variable.
    pub fn reg<S: AsRef<str>>(&self, s: S) -> Reg {
        unsafe {
            let c_name = CString::new(s.as_ref()).unwrap();
            let c_name_ptr = c_name.into_raw();
            MIR_reg(self.module.ctx.ctx, c_name_ptr, (*self.ptr).u.func)
        }
    }

    /// Create a new label within the context
    pub fn label(&'module self) -> Label {
        Label {
            ptr: unsafe { MIR_new_label(self.module.ctx.ctx) },
        }
    }

    fn new_op(&self, op: Op) -> MIR_op_t {
        unsafe {
            match op {
                Op::Reg(r) => MIR_new_reg_op(self.module.ctx.ctx, r),
                Op::Int(i) => MIR_new_int_op(self.module.ctx.ctx, i),
                Op::Label(l) => MIR_new_label_op(self.module.ctx.ctx, l.ptr),
                _ => todo!(),
            }
        }
    }

    /// Append a new instruction to the function
    pub fn append_insn(&mut self, insn: Insn) {
        unsafe {
            match insn {
                Insn::Mov(reg, int) => MIR_append_insn(
                    self.module.ctx.ctx,
                    self.ptr,
                    MIR_new_insn(
                        self.module.ctx.ctx,
                        MIR_insn_code_t_MIR_MOV,
                        self.new_op(reg),
                        self.new_op(int),
                    ),
                ),
                Insn::Bge(x, y, z) => MIR_append_insn(
                    self.module.ctx.ctx,
                    self.ptr,
                    MIR_new_insn(
                        self.module.ctx.ctx,
                        MIR_insn_code_t_MIR_BGE,
                        self.new_op(x),
                        self.new_op(y),
                        self.new_op(z),
                    ),
                ),
                Insn::Label(l) => MIR_append_insn(self.module.ctx.ctx, self.ptr, l.ptr),
                Insn::Add(x, y, z) => MIR_append_insn(
                    self.module.ctx.ctx,
                    self.ptr,
                    MIR_new_insn(
                        self.module.ctx.ctx,
                        MIR_insn_code_t_MIR_ADD,
                        self.new_op(x),
                        self.new_op(y),
                        self.new_op(z),
                    ),
                ),
                Insn::Blt(x, y, z) => MIR_append_insn(
                    self.module.ctx.ctx,
                    self.ptr,
                    MIR_new_insn(
                        self.module.ctx.ctx,
                        MIR_insn_code_t_MIR_BLT,
                        self.new_op(x),
                        self.new_op(y),
                        self.new_op(z),
                    ),
                ),
                Insn::Ret(ops) => {
                    let nops = ops.len();
                    let mut ops: Vec<MIR_op_t> = ops.iter().map(|x| self.new_op(*x)).collect();
                    let op_ptr = ops.as_mut_ptr();
                    MIR_append_insn(
                        self.module.ctx.ctx,
                        self.ptr,
                        MIR_new_ret_insn(self.module.ctx.ctx, nops, op_ptr),
                    )
                }
                _ => todo!(),
            }
        }
    }

    pub fn append_insns(&mut self, insns: &[Insn]) {
        for insn in insns {
            self.append_insn(insn.clone())
        }
    }
}

/// [`Data`] represents the following MIR constants:
/// + [`MIR_type_t_MIR_T_BLK`]
/// + [`MIR_type_t_MIR_T_BOUND`]
/// + [`MIR_type_t_MIR_T_D`]
/// + [`MIR_type_t_MIR_T_F`]
/// + [`MIR_type_t_MIR_T_I8`]
/// + [`MIR_type_t_MIR_T_I16`]
/// + [`MIR_type_t_MIR_T_I32`]
/// + [`MIR_type_t_MIR_T_I64`]
/// + [`MIR_type_t_MIR_T_LD`]
/// + [`MIR_type_t_MIR_T_P`]
/// + [`MIR_type_t_MIR_T_RBLK`]
/// + [`MIR_type_t_MIR_T_U8`]
/// + [`MIR_type_t_MIR_T_U16`]
/// + [`MIR_type_t_MIR_T_U32`]
/// + [`MIR_type_t_MIR_T_U64`]
/// + [`MIR_type_t_MIR_T_UNDEF`]
pub enum Data {
    Blk = MIR_type_t_MIR_T_BLK as isize,
    Bound = MIR_type_t_MIR_T_BOUND as isize,
    D = MIR_type_t_MIR_T_D as isize,
    F = MIR_type_t_MIR_T_F as isize,
    I8 = MIR_type_t_MIR_T_I8 as isize,
    I16 = MIR_type_t_MIR_T_I16 as isize,
    I32 = MIR_type_t_MIR_T_I32 as isize,
    I64 = MIR_type_t_MIR_T_I64 as isize,
    Ld = MIR_type_t_MIR_T_LD as isize,
    P = MIR_type_t_MIR_T_P as isize,
    Rblk = MIR_type_t_MIR_T_RBLK as isize,
    U8 = MIR_type_t_MIR_T_U8 as isize,
    U16 = MIR_type_t_MIR_T_U16 as isize,
    U32 = MIR_type_t_MIR_T_U32 as isize,
    U64 = MIR_type_t_MIR_T_U64 as isize,
    Undef = MIR_type_t_MIR_T_UNDEF as isize,
}

impl From<&Data> for u32 {
    fn from(value: &Data) -> Self {
        match value {
            Data::Blk => MIR_type_t_MIR_T_BLK,
            Data::Bound => MIR_type_t_MIR_T_BOUND,
            Data::D => MIR_type_t_MIR_T_D,
            Data::F => MIR_type_t_MIR_T_F,
            Data::I8 => MIR_type_t_MIR_T_I8,
            Data::I16 => MIR_type_t_MIR_T_I16,
            Data::I32 => MIR_type_t_MIR_T_I32,
            Data::I64 => MIR_type_t_MIR_T_I64,
            Data::Ld => MIR_type_t_MIR_T_LD,
            Data::P => MIR_type_t_MIR_T_P,
            Data::Rblk => MIR_type_t_MIR_T_RBLK,
            Data::U8 => MIR_type_t_MIR_T_U8,
            Data::U16 => MIR_type_t_MIR_T_U16,
            Data::U32 => MIR_type_t_MIR_T_U32,
            Data::U64 => MIR_type_t_MIR_T_U64,
            Data::Undef => MIR_type_t_MIR_T_UNDEF,
        }
    }
}

impl From<Data> for u32 {
    fn from(value: Data) -> Self {
        match value {
            Data::Blk => MIR_type_t_MIR_T_BLK,
            Data::Bound => MIR_type_t_MIR_T_BOUND,
            Data::D => MIR_type_t_MIR_T_D,
            Data::F => MIR_type_t_MIR_T_F,
            Data::I8 => MIR_type_t_MIR_T_I8,
            Data::I16 => MIR_type_t_MIR_T_I16,
            Data::I32 => MIR_type_t_MIR_T_I32,
            Data::I64 => MIR_type_t_MIR_T_I64,
            Data::Ld => MIR_type_t_MIR_T_LD,
            Data::P => MIR_type_t_MIR_T_P,
            Data::Rblk => MIR_type_t_MIR_T_RBLK,
            Data::U8 => MIR_type_t_MIR_T_U8,
            Data::U16 => MIR_type_t_MIR_T_U16,
            Data::U32 => MIR_type_t_MIR_T_U32,
            Data::U64 => MIR_type_t_MIR_T_U64,
            Data::Undef => MIR_type_t_MIR_T_UNDEF,
        }
    }
}

/// Some register address.
pub type Reg = u32;

/// Thin wrapper around `MIR_var`.
#[repr(transparent)]
pub struct Var<'func> {
    marker: PhantomData<&'func Var<'func>>,
    var: MIR_var_t,
}

impl<'func> Var<'func> {
    pub fn new<S: AsRef<str>>(data: Data, name: S) -> Self {
        unsafe {
            let c_name = CString::new(name.as_ref()).unwrap();
            let c_name_ptr = c_name.into_raw();
            let var = MIR_var_t {
                name: c_name_ptr,
                type_: data.into(),
                size: 0,
            };
            Self {
                marker: PhantomData,
                var,
            }
        }
    }
}

/// Label which makes up the code blocks within functions themselves;
/// in the C-API this is a kind of `insn`, however, here they are treated specially
/// as they are used in different ways as ops.
#[derive(Copy, Clone)]
pub struct Label {
    ptr: *mut MIR_insn,
}

/// All MIR [`Insn`], barring [`Insn::Ret`], and [`Insn::Call`] expect a fixed number of operands.
#[derive(Clone)]
pub enum Insn<'a> {
    Mov(Op, Op),
    Bge(Op, Op, Op),
    Label(Label),
    Add(Op, Op, Op),
    Blt(Op, Op, Op),
    Ret(&'a [Op]),
    // TODO add more insns
}

#[derive(Copy, Clone)]
pub enum Op {
    Int(i64),
    Label(Label),
    Reg(Reg),
    UInt(u64),
    Float(f32),
    Double(f64),
    Str(Str),
    // TODO add more opcodes
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Str {
    str: MIR_str
}
