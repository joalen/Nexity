use inkwell::context::Context;
use inkwell::types::BasicTypeEnum;
use crate::ast::ast::Type;

pub fn to_llvm_type<'ctx>(context: &'ctx Context, ty: &Type) -> BasicTypeEnum<'ctx> {
    match ty {
        Type::Int  => context.i64_type().into(),
        Type::Float => context.f64_type().into(),
        Type::Bool  => context.bool_type().into(),
        Type::Char  => context.i8_type().into(),
        Type::Function(_, _) => context.ptr_type(inkwell::AddressSpace::default()).into(),
        _ => context.i64_type().into(), // fallback
    }
}