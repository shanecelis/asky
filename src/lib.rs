#![forbid(missing_docs)]
//! Good looking prompts for the terminal
//!
//! # Available prompts
//!
//! - [`Confirm`] - Ask yes/no questions.
//! - [`Toggle`] - Choose between two options.
//! - [`Text`] - One-line user input.
//! - [`Number`] - One-line user input of numbers.
//! - [`Password`] - One-line user input as password.
//! - [`Select`] - Select an item from a list.
//! - [`MultiSelect`] - Select multiple items from a list.
//!
//! # Simple Example
//!
//! ```rust, no_run
//! use asky::prelude::*;
//!
//! fn main() -> Result<(), Error> {
//! #   #[cfg(feature = "terminal")]
//!     let name = Text::new("Hi. What's your name?").prompt()?;
//!
//! #   #[cfg(feature = "terminal")]
//!     if Confirm::new("Do you like coffee?").prompt()? {
//!         println!("Great! Me too");
//!     } else {
//!         println!("Hmm... Interesting");
//!     }
//!
//!     // ...
//!
//!     Ok(())
//! }
//! ```
//!
//! # Customization
//!
//! If you'd like to use this crate but don't want the default styles or just want to customize as you like,
//! all the prompts allow setting a custom formatter using `format()` method.
//!
//! The formatter receives a prompt state reference and a [`DrawTime`],
//! and returns the string to display in the terminal.
//!
//! > Note: When using a custom formatter, you are responsible for the presentation of the prompt,
//! > so you must handle the colors, icons, etc. by yourself.
//!
//! #### Example
//!
//! ```rust, no_run
//! # use asky::prelude::*;
//! # fn main() -> Result<(), Error> {
//! # #[cfg(feature = "terminal")]
//! Confirm::new("Do you like Rust?")
//!     .format(|prompt, out| {
//!         let state = if prompt.active { "Y/n" } else { "y/N" };
//!         write!(out, "{} {}\n", prompt.message, state)
//!     })
//!     .prompt();
//! # Ok(())
//! # }
//! ```
//!
//! This will prints
//!
//! ```bash
//! Do you like Rust? y/N
//! ```
//!
//! ## Cursor Position
//!
//! Almost all the prompts just need a custom string, but some prompts like [`Text`] also requires an array of `[x, y]`
//! position for the cursor, due to these prompts also depends on the cursor position in the process.
//!
//! #### Example
//!
//! ```rust, no_run
//! # use asky::{Text, Error, Promptable, Printable};
//! # fn main() -> Result<(), Error> {
//! # #[cfg(feature = "terminal")]
//! Text::new("What is your name")
//!     .format(|prompt, out| {
//!         let cursor_col = prompt.input.col;
//!         let prefix = "> ";
//!
//!         let x = (prefix.len() + cursor_col);
//!         let y = 1;
//!
//!         write!(out, "{}\n{} {}", prompt.message, prefix, prompt.input.value)
//!     })
//!     .prompt();
//! # Ok(())
//! # }
//!
//! ```
//!
//! This will prints
//!
//! ```bash
//! What is your name?
//! > |
//! ```
//!
//! Where `|` is the cursor position.
// #![deny(missing_docs)]
#![cfg_attr(not(feature = "terminal"), allow(dead_code))]
mod prompts;
pub mod utils;
use std::borrow::Cow;

/// Defines object which has a principle value or error.
///
/// NOTE: Does not define the object as having submitted its value for the
/// purpose of Asky.
pub trait Valuable {
    /// Value type
    type Output: Send;
    /// Return value or error.
    fn value(&self) -> Result<Self::Output, Error>;
}

/// Set a value.
pub trait SetValue {
    /// Value type
    type Output: Send;
    /// Set a value or error.
    fn set_value(&mut self, value: Self::Output) -> Result<(), Error>;
}


/// What to do on each tick.
///
/// # Motivation
///
/// This allows Asky nodes to abort or finish without dependence on input.
#[derive(Debug, Clone, Copy)]
pub enum OnTick {
    /// Continue
    Continue,
    /// Complete
    Finish,
    /// Abort
    Abort
}

/// A tick on each frame.
pub trait Tick {
    /// What to do on each frame. By default it will return [OnTick::Continue].
    fn tick(&mut self) -> OnTick {
        OnTick::Continue
    }
}


/// Asky errors
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// User cancelled.
    #[error("cancelled")]
    Cancel,
    /// Input was invalid.
    #[error("invalid input")]
    InvalidInput,
    /// Invalid count with expected and actual.
    #[error("invalid count, expected {expected} actual {actual}")]
    InvalidCount {
        /// Expected count
        expected: usize,
        /// Actual count
        actual: usize
    },
    /// Validation failed.
    #[error("validation fail")]
    ValidationFail,
    /// Message
    #[error("{0}")]
    Message(Cow<'static, str>),
    /// There was an [std::io::Error].
    #[error("io error {0}")]
    Io(#[from] std::io::Error),
    #[cfg(feature = "bevy")]
    /// Async error
    #[error("async error {0}")]
    Async(#[from] bevy_defer::AsyncFailure),
}

/// A prompt
///
/// TODO: Rename to Prompt
pub trait Promptable {
    /// Output type for a successful prompt.
    type Output;
    /// Prompt the user for input.
    fn prompt(&mut self) -> Result<Self::Output, crate::Error>;
}

pub use prompts::confirm::Confirm;
pub use prompts::message::Message;
pub use prompts::multi_select::MultiSelect;
pub use prompts::number::Number;
pub use prompts::password::Password;
pub use prompts::select::Select;
pub use prompts::text::Text;
pub use prompts::toggle::Toggle;

pub use prompts::select::{SelectInput, SelectOption};
pub use prompts::text::LineInput;
pub use utils::key_listener::Typeable;
pub use utils::num_like::NumLike;
pub use utils::renderer::{DrawTime, Printable};

/// Prelude
pub mod prelude {
    pub use super::{utils::renderer::Printable, Error, Promptable, SelectOption, Valuable};
    pub use super::{Confirm, Message, MultiSelect, Number, Password, Select, Text, Toggle};
}

#[cfg(feature = "terminal")]
mod terminal;

#[cfg(feature = "bevy")]
pub mod bevy;
#[cfg(feature = "bevy")]
mod text_style_adapter;

pub mod style;
