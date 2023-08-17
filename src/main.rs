#![allow(unused, dead_code)]

mod def;
mod expr;
mod mir;

use std::fs::File;

use tinyvec::array_vec;

use crate::mir::*;

fn main() {
    let mut ctx = Context::init();
    let mut m = ctx.new_module("m");
    let mut func = m.new_function(
        "loop",
        &[Data::I64],
        &[Var::new(Data::I64, "arg1"), Var::new(Data::I64, "arg2")],
    );
    let count_reg = func.func_reg(Data::I64, "count");
    let arg1_reg = func.reg("arg1");

    let (fin, cont) = (func.label(), func.label());

    func.append_insns(&[
        Insn::Mov(Op::Reg(count_reg), Op::Int(0)),
        Insn::Bge(Op::Label(fin), Op::Reg(count_reg), Op::Reg(arg1_reg)),
        Insn::Label(cont),
        Insn::Add(Op::Reg(count_reg), Op::Reg(count_reg), Op::Reg(arg1_reg)),
        Insn::Blt(Op::Label(cont), Op::Reg(count_reg), Op::Reg(arg1_reg)),
        Insn::Label(fin),
        Insn::Ret(&[Op::Reg(count_reg)]),
    ]);

    m.finish_function(func);
    ctx.finish_module(m);

    ctx.write_out();
}
