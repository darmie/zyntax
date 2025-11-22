//! Error types for ZynPEG

use thiserror::Error;

/// Errors that can occur during .zyn grammar parsing and code generation
#[derive(Error, Debug)]
pub enum ZynPegError {
    #[error("Grammar parse error: {0}")]
    ParseError(String),

    #[error("Invalid action block: {0}")]
    InvalidAction(String),

    #[error("Unknown rule reference: {0}")]
    UnknownRule(String),

    #[error("Code generation error: {0}")]
    CodeGenError(String),

    #[error("Type error in action: {0}")]
    TypeError(String),

    #[error("Missing required directive: {0}")]
    MissingDirective(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

impl From<pest::error::Error<crate::Rule>> for ZynPegError {
    fn from(err: pest::error::Error<crate::Rule>) -> Self {
        ZynPegError::ParseError(err.to_string())
    }
}

pub type Result<T> = std::result::Result<T, ZynPegError>;
