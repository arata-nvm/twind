use std::{cell::RefCell, fmt};

use crate::interpreter::parser::BinaryOperator;

use super::{environment, error::InterpreterError, parser::Expression};

pub type Environment = environment::Environment<String, Type>;
pub type TypeVariable = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Boolean,
    Integer,
    Function(Box<Type>, Box<Type>),
    Variable(TypeVariable),
    Schema(TypeVariable),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Boolean => write!(f, "bool"),
            Type::Integer => write!(f, "int"),
            Type::Function(param, ret) => write!(f, "({param} -> {ret})"),
            Type::Variable(index) => write!(f, "{index}"),
            Type::Schema(index) => write!(f, "{index}"),
        }
    }
}

pub fn infer(expr: Expression, tenv: &mut Environment) -> Result<Type, InterpreterError> {
    match expr {
        Expression::Identifier(name) => match tenv.lookup(&name) {
            Some(typ) => Ok(instanciate(subst(typ, tenv), tenv)),
            None => Err(InterpreterError::CannotFindVariable { name }),
        },
        Expression::Boolean(_) => Ok(Type::Boolean),
        Expression::Integer(_) => Ok(Type::Integer),
        Expression::Binary(op, lhs, rhs) => {
            let lhs = infer(*lhs, tenv)?;
            let rhs = infer(*rhs, tenv)?;
            unify(lhs, Type::Integer, tenv)?;
            unify(rhs, Type::Integer, tenv)?;
            match op {
                BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Mul
                | BinaryOperator::Div => Ok(Type::Integer),
                BinaryOperator::Lt => Ok(Type::Boolean),
            }
        }
        Expression::If(cond, val_then, val_else) => {
            let cond = infer(*cond, tenv)?;
            let val_then = infer(*val_then, tenv)?;
            let val_else = infer(*val_else, tenv)?;
            unify(cond, Type::Boolean, tenv)?;
            unify(val_then.clone(), val_else, tenv)?;
            Ok(subst(val_then, tenv))
        }
        Expression::Let(name, expr_to_bind, expr) => {
            let expr_to_bind = generalize(infer(*expr_to_bind, tenv)?);
            if let Some(expr) = expr {
                return infer(*expr, &mut tenv.expanded(name, expr_to_bind));
            }

            tenv.expand(name, expr_to_bind);
            Ok(Type::Void)
        }
        Expression::LetRec(name, expr_to_bind, expr) => {
            let param = new_type_var();
            let ret = new_type_var();
            let func = Type::Function(Box::new(param.clone()), Box::new(ret.clone()));

            let mut newtenv = tenv.expanded(name.clone(), func);
            let func2 = infer(*expr_to_bind, &mut newtenv)?;
            let Type::Function(param2, ret2) = func2.clone() else {
                return Err(InterpreterError::UnexpectedType { expect: "function".to_string(), found: Some(func2) });
            };

            unify(param, *param2, tenv)?;
            unify(ret, *ret2, tenv)?;

            if let Some(expr) = expr {
                return infer(*expr, &mut tenv.expanded(name, func2));
            }

            Ok(Type::Void)
        }
        Expression::Function(param_name, expr) => {
            let param = new_type_var();
            tenv.expand(param_name, param.clone());
            let newtenv = tenv;
            let expr = infer(*expr, newtenv)?;

            Ok(Type::Function(
                Box::new(subst(param, newtenv)),
                Box::new(subst(expr, newtenv)),
            ))
        }
        Expression::Apply(func, arg) => {
            let func = infer(*func, tenv)?;
            let arg = infer(*arg, tenv)?;
            let ret = new_type_var();
            unify(
                func,
                Type::Function(Box::new(arg), Box::new(ret.clone())),
                tenv,
            )?;
            Ok(subst(ret, tenv))
        }
        Expression::OperatorFunction(_) => todo!(),
    }
}

fn instanciate(typ: Type, tenv: &mut Environment) -> Type {
    match typ {
        Type::Void | Type::Boolean | Type::Integer | Type::Variable(_) => typ,
        Type::Function(param, ret) => {
            let param = instanciate(*param, tenv);
            let ret = instanciate(*ret, tenv);
            Type::Function(Box::new(param), Box::new(ret))
        }
        Type::Schema(schema_var) => match tenv.lookup(&schema_var) {
            Some(typ_var) => typ_var,
            None => {
                let typ_var = new_type_var();
                tenv.expand(schema_var, typ_var.clone());
                typ_var
            }
        },
    }
}

fn generalize(typ: Type) -> Type {
    do_generalize(typ, &mut Environment::new())
}

fn do_generalize(typ: Type, tenv: &mut Environment) -> Type {
    match typ {
        Type::Void | Type::Boolean | Type::Integer | Type::Schema(_) => typ,
        Type::Function(param, ret) => {
            let param = do_generalize(*param, tenv);
            let ret = do_generalize(*ret, tenv);
            Type::Function(Box::new(param), Box::new(ret))
        }
        Type::Variable(var) => match tenv.lookup(&var) {
            Some(schema) => schema,
            None => {
                let schema = new_type_schema();
                tenv.expand(var, schema.clone());
                schema
            }
        },
    }
}

fn unify(typ1: Type, typ2: Type, tenv: &mut Environment) -> Result<(), InterpreterError> {
    match (typ1, typ2) {
        (Type::Void, Type::Void)
        | (Type::Boolean, Type::Boolean)
        | (Type::Integer, Type::Integer) => Ok(()),
        (Type::Function(param1, ret1), Type::Function(param2, ret2)) => {
            unify(*param1, *param2, tenv)?;
            unify(*ret1, *ret2, tenv)?;
            Ok(())
        }

        (typ, Type::Variable(var)) | (Type::Variable(var), typ) => {
            tenv.expand(var, typ);
            Ok(())
        }

        (typ1, typ2) => Err(InterpreterError::CannotUnifyType { typ1, typ2 }),
    }
}

fn subst(typ: Type, tenv: &Environment) -> Type {
    match typ {
        Type::Void | Type::Boolean | Type::Integer | Type::Schema(_) => typ,
        Type::Function(param, ret) => {
            let param = subst(*param, tenv);
            let ret = subst(*ret, tenv);
            Type::Function(Box::new(param), Box::new(ret))
        }
        Type::Variable(ref var) => match tenv.lookup(var) {
            Some(typ) => subst(typ, tenv),
            None => typ,
        },
    }
}

fn new_type_var() -> Type {
    thread_local! {
        static INDEX: RefCell<usize> = RefCell::new(0);
    }

    INDEX.with(|index| {
        let typ = Type::Variable(format!("'{}", *index.borrow()));
        *index.borrow_mut() += 1;
        typ
    })
}

fn new_type_schema() -> Type {
    thread_local! {
        static INDEX: RefCell<usize> = RefCell::new(0);
    }

    INDEX.with(|index| {
        let typ = Type::Schema(format!("''{}", *index.borrow()));
        *index.borrow_mut() += 1;
        typ
    })
}
