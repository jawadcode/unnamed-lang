///! # Type inference in less than 100 lines of Rust
///!
///! - Do with it what you will
///! - Licensed under (https://en.wikipedia.org/wiki/WTFPL)
///!
///! ~ zesterer
use std::{collections::HashMap, fmt::Display};

/// A concrete type that has been fully inferred
#[derive(Debug)]
enum Type {
    /// A boolean
    Bool,
    /// A number
    Num,
    /// A string
    Str,
    /// A tuple
    Tuple(Box<Type>, Box<Type>),
    // /// A list
    // List(Box<Type>),
    // /// A function
    // Fun(/* in */ Box<Type>, /* out */ Box<Type>),
}

/// A identifier to uniquely refer to our type terms
pub type TypeId = usize;

/// Information about a type term
#[derive(Clone, Debug)]
enum TypeInfo {
    /// No type information
    Unknown,
    /// Same as another type term
    Ref(TypeId),
    /// A boolean
    Bool,
    /// A number
    Num,
    /// A string
    Str,
    /// A tuple
    Tuple(TypeId, TypeId),
    // /// A list
    // List(TypeId),
    // /// A function
    // Fun(/* in */ TypeId, /* out */ TypeId),
}

/// An error that could arise during type inference
enum InferenceError {
    Conflict(TypeInfo, TypeInfo),
    CouldNotInfer,
}

#[derive(Default)]
struct Engine {
    id_counter: usize, // Used to generate unique IDs
    vars: HashMap<TypeId, TypeInfo>,
}

impl Engine {
    /// Infer `Type` of `node`
    pub fn infer(&mut self, node: &Node) -> Result<Type, InferenceError> {
        let id = self.get_typeid(node);
        self.reconstruct(id)
    }

    /// Get `TypeId` of `node`
    fn get_typeid(&mut self, node: &Node) -> TypeId {
        match node {
            Node::Bool(boolean) => self.insert(TypeInfo::Bool),
            Node::Num(number) => self.insert(TypeInfo::Num),
            Node::Str(string) => self.insert(TypeInfo::Str),
            Node::Tuple(left, right) => {
                let left = self.get_typeid(left);
                let right = self.get_typeid(right);
                self.insert(TypeInfo::Tuple(left, right))
            } // Node::List(items) => self.insert(TypeInfo::List(self.get_typeid(items))),
              // Node::Fun(_, input, output) => {
              //     let input = self.get_typeid(input);
              //     let output = self.get_typeid(output);
              //     self.insert(TypeInfo::Fun(input, output))
              // }
        }
    }

    /// Create a new type term with whatever we have about its type
    fn insert(&mut self, info: TypeInfo) -> TypeId {
        // Generate a new ID for our type term
        self.id_counter += 1;
        let id = self.id_counter;
        self.vars.insert(id, info);
        id
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), InferenceError> {
        use TypeInfo::*;
        match (self.vars[&a].clone(), self.vars[&b].clone()) {
            // Follow any references
            (Ref(a), _) => self.unify(a, b),
            (_, Ref(b)) => self.unify(a, b),

            // When we don't know anything about either term, assume that
            // they match and make the one we know nothing about reference the
            // one we may know something about
            (Unknown, _) => {
                self.vars.insert(a, TypeInfo::Ref(b));
                Ok(())
            }
            (_, Unknown) => {
                self.vars.insert(b, TypeInfo::Ref(a));
                Ok(())
            }

            // Primitives are trivial to unify
            (Bool, Bool) => Ok(()),
            (Num, Num) => Ok(()),
            (Str, Str) => Ok(()),

            // When unifying complex types, we must check their sub-types. This
            // can be trivially implemented for tuples, sum types, etc.
            (Tuple(a1, b1), Tuple(a2, b2)) => self.unify(a1, a2).and_then(|_| self.unify(b1, b2)),
            // (List(a_item), List(b_item)) => self.unify(a_item, b_item),
            // (Fun(a_i, a_o), Fun(b_i, b_o)) => {
            //     self.unify(a_i, b_i).and_then(|_| self.unify(a_o, b_o))
            // }

            // If no previous attempts to unify were successful, raise an error
            (a, b) => Err(InferenceError::Conflict(a, b)),
        }
    }

    /// Attempt to reconstruct a concrete type from the given type term ID. This
    /// may fail if we don't yet have enough information to figure out what the
    /// type is.
    fn reconstruct(&self, id: TypeId) -> Result<Type, InferenceError> {
        use TypeInfo::*;
        match self.vars[&id] {
            Unknown => Err(InferenceError::CouldNotInfer),
            Ref(id) => self.reconstruct(id),
            Bool => Ok(Type::Bool),
            Num => Ok(Type::Num),
            Str => Ok(Type::Str),

            Tuple(a, b) => Ok(Type::Tuple(
                Box::new(self.reconstruct(a)?),
                Box::new(self.reconstruct(b)?),
            )),
            // List(item) => Ok(Type::List(Box::new(self.reconstruct(item)?))),
            // Fun(i, o) => Ok(Type::Fun(
            //     Box::new(self.reconstruct(i)?),
            //     Box::new(self.reconstruct(o)?),
            // )),
        }
    }
}

impl Display for InferenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferenceError::Conflict(a, b) => {
                write!(f, "Type conflict between {:?} and {:?}", a, b)
            }
            InferenceError::CouldNotInfer => write!(f, "Could not infer type"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "Boolean"),
            Type::Num => write!(f, "Number"),
            Type::Str => write!(f, "String"),
            Type::Tuple(a, b) => write!(f, "Tuple[{a}, {b}]"),
        }
    }
}

/// AST Node
#[derive(Debug, Clone)]
enum Node {
    Bool(bool),
    Num(f64),
    Str(String),
    Tuple(Box<Node>, Box<Node>),
    // List(Vec<Node>),
    // Fun(String, Box<Node>, Box<Node>),
}

#[cfg(test)]
mod tests {
    use crate::typeck::Engine;

    use super::Node;

    fn sexify(node: &Node, indent: usize) -> String {
        match node {
            Node::Bool(boolean) => if *boolean { "true" } else { "false" }.to_string(),
            Node::Num(number) => number.to_string(),
            Node::Str(string) => format!("\"{string}\""),
            Node::Tuple(left, right) => format!(
                "(tuple\n{ind}{}\n{ind}{})",
                sexify(left, indent + 1),
                sexify(right, indent + 1),
                ind = "  ".repeat(indent + 1),
            ),
        }
    }

    #[test]
    fn basic_infer() {
        let ast = Node::Tuple(
            Box::new(Node::Num(123.4)),
            Box::new(Node::Tuple(
                Box::new(Node::Bool(true)),
                Box::new(Node::Str("big funny".to_string())),
            )),
        );
        println!("\nAST:\n{}\n", sexify(&ast, 0));

        let mut engine = Engine::default();
        match engine.infer(&ast) {
            Ok(typ) => println!("Type: {typ}"),
            Err(err) => eprintln!("Error: {err}"),
        }
    }
}
