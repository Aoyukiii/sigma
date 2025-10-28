use std::collections::HashMap;

use crate::core::{
    syntax::ast::{
        elaborated::{
            self,
            id::{DebruijnId, ExprId, StringId},
        },
        raw,
    },
    utils::span::Span,
};

#[derive(Debug)]
pub struct Env<'a, T> {
    parent: Option<(&'a Env<'a, T>, T)>,
} // How to use linked list here?

impl<'a, T> Default for Env<'a, T> {
    fn default() -> Self {
        Self { parent: None }
    }
}

impl<'a, T> Env<'a, T> {
    pub fn with(&'a self, data: T) -> Self {
        Self {
            parent: Some((&self, data)),
        }
    }
}

pub struct Context {
    id_used_count: usize,
    type_info: Vec<elaborated::TyExpr>,
    symbol_table: Vec<String>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            id_used_count: 0,
            type_info: Vec::new(),
            symbol_table: Vec::new(),
        }
    }

    pub fn get_new_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.id_used_count);
        self.id_used_count += 1;
        id
    }

    pub fn set_type(&mut self, id: ExprId, ty: elaborated::TyExpr) {
        let index = id.0;
        if index >= self.type_info.len() {
            self.type_info
                .resize_with(index + 1, elaborated::TyExpr::default);
        }
        self.type_info[index] = ty;
    }

    pub fn get_type(&self, id: ExprId) -> Option<&elaborated::TyExpr> {
        self.type_info.get(id.0)
    }

    pub fn get_mut_type(&mut self, id: ExprId) -> Option<&mut elaborated::TyExpr> {
        self.type_info.get_mut(id.0)
    }

    pub fn create_new_string(&mut self, s: String) -> StringId {
        let id = StringId(self.symbol_table.len());
        self.symbol_table.push(s);
        id
    }

    pub fn get_string(&mut self, id: StringId) -> String {
        self.symbol_table[id.0].clone()
    }
}

fn raw_to_elaborated(
    raw: &raw::Expr,
    ctx: &mut Context,
    env: &Env<(StringId, DebruijnId)>,
) -> elaborated::Expr {
    todo!()
}

fn raw_to_elaborated_kind(
    raw: &raw::Expr,
    ctx: &mut Context,
    env: &Env<(StringId, DebruijnId)>,
) -> elaborated::ExprKind {
    use raw::ExprKind::*;
    let kind = match &raw.kind {
        Ident(name) => elaborate_ident(name.clone(), env),
        Atom => elaborated::ExprKind::Atom,
        Type => elaborated::ExprKind::Type,
        AtomLiteral(value) => elaborated::ExprKind::AtomLiteral(value.clone()),
        Annotated(it) => {
            return todo!();
            // let expr = raw_to_elaborated(&it.expr, ctx, env);
            // let type_expr = raw_to_elaborated_kind(&it.type_expr, ctx, env);
            // ctx.set_type(expr.id, type_expr);
            // return expr;
        }
        // Lambda(it) => ...,
        Application(it) => {
            elaborated::ExprKind::Application(Box::new(elaborated::expr::Application {
                func: Box::new(raw_to_elaborated(&it.func, ctx, env)),
                arg: Box::new(raw_to_elaborated(&it.arg, ctx, env)),
            }))
        }
        Prefix(it) => {
            let op = (
                elaborate_ident(it.op.to_string(), env),
                it.op_span,
                ctx.get_new_expr_id(),
            )
                .into();
            let rhs = raw_to_elaborated(&it.rhs, ctx, env);
            let appl = elaborated::expr::Application {
                func: Box::new(op),
                arg: Box::new(rhs),
            };
            elaborated::ExprKind::Application(Box::new(appl))
        }
        Infix(it) => {
            let op: elaborated::Expr = (
                elaborate_ident(it.op.to_string(), env),
                it.op_span,
                ctx.get_new_expr_id(),
            )
                .into();
            let lhs = raw_to_elaborated(&it.lhs, ctx, env);
            let rhs = raw_to_elaborated(&it.rhs, ctx, env);
            let appl_left_span = op.span.merge(lhs.span);
            let appl_left =
                elaborated::ExprKind::Application(Box::new(elaborated::expr::Application {
                    func: Box::new(op),
                    arg: Box::new(lhs),
                }));
            let appl = elaborated::expr::Application {
                func: Box::new((appl_left, appl_left_span, ctx.get_new_expr_id()).into()),
                arg: Box::new(rhs),
            };
            elaborated::ExprKind::Application(Box::new(appl))
        }
        // Let(it) => ...,
        Error => elaborated::ExprKind::Error,
        _ => unreachable!(),
    };
    kind
}

fn elaborate_ident(name: String, env: &Env<(StringId, DebruijnId)>) -> elaborated::ExprKind {
    let debruijn_id = todo!();
    elaborated::ExprKind::Var(debruijn_id)
}
