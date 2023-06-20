use std::io;

#[cfg(feature="bevy")]
use bevy::prelude::*;
#[cfg(feature="bevy")]
use crate::bevy::*;

#[cfg(feature="terminal")]
use crossterm::event::{KeyCode, KeyEvent};

use crate::utils::key_listener::Typeable;
#[cfg(feature="terminal")]
use crate::utils::key_listener::self;
use crate::utils::{
    renderer::{DrawTime, Printable, Renderer},
    theme,
};

use colored::{Colorize, ColoredString, ColoredStrings};
use super::text::{Direction, InputValidator, LineInput};

type Formatter<'a> = dyn Fn(&Password, DrawTime, &mut ColoredStrings) -> [usize; 2] + 'a + Send + Sync;

/// Prompt to get one-line user input as password.
///
/// Similar to [`Text`] prompt, but replace input characters with `*`.
/// Also allow to hide user input completely.
///
/// # Key Events
///
/// | Key         | Action                       |
/// | ----------- | ---------------------------- |
/// | `Enter`     | Submit current/initial value |
/// | `Backspace` | Delete previous character    |
/// | `Delete`    | Delete current character     |
/// | `Left`      | Move cursor left             |
/// | `Right`     | Move cursor right            |
///
/// # Examples
///
/// ```no_run
/// use asky::Password;
///
/// # fn main() -> std::io::Result<()> {
/// let password = Password::new("Your IG Password:").prompt()?;
/// # Ok(())
/// # }
/// ```
/// [`Text`]: crate::Text
pub struct Password<'a> {
    /// Message used to display in the prompt.
    pub message: &'a str,
    /// Input state for the prompt.
    pub input: LineInput,
    /// Placeholder to show when the input is empty.
    pub placeholder: Option<&'a str>,
    /// Default value to submit when the input is empty.
    pub default_value: Option<&'a str>,
    /// Must hide user input or show `*` characters
    pub hidden: bool,
    /// State of the validation of the user input.
    pub validator_result: Result<(), &'a str>,
    validator: Option<Box<InputValidator<'a>>>,
    formatter: Box<Formatter<'a>>,
}

impl<'a> Password<'a> {
    /// Create a new password prompt.
    pub fn new(message: &'a str) -> Self {
        Password {
            message,
            input: LineInput::new(),
            placeholder: None,
            default_value: None,
            hidden: false,
            validator: None,
            validator_result: Ok(()),
            formatter: Box::new(theme::fmt_password2),
        }
    }

    /// Set text to show when the input is empty.
    ///
    /// This not will not be submitted when the input is empty.
    pub fn placeholder(&mut self, value: &'a str) -> &mut Self {
        self.placeholder = Some(value);
        self
    }

    /// Set default value to submit when the input is empty.
    pub fn default(&mut self, value: &'a str) -> &mut Self {
        self.default_value = Some(value);
        self
    }

    /// Set initial value, could be deleted by the user.
    pub fn initial(&mut self, value: &str) -> &mut Self {
        self.input.set_value(value);
        self
    }

    /// Set whether to hide user input or show `*` characters
    pub fn hidden(&mut self, hidden: bool) -> &mut Self {
        self.hidden = hidden;
        self
    }

    /// Set validator to the user input.
    pub fn validate<F>(&mut self, validator: F) -> &mut Self
    where
        F: Fn(&str) -> Result<(), &'a str> + 'a + Send + Sync,
    {
        self.validator = Some(Box::new(validator));
        self
    }

    /// Set custom closure to format the prompt.
    ///
    /// See: [`Customization`](index.html#customization).
    pub fn format<F>(&mut self, formatter: F) -> &mut Self
    where
        F: Fn(&Password, DrawTime, &mut ColoredStrings) -> [usize; 2] + 'a + Send + Sync,
    {
        self.formatter = Box::new(formatter);
        self
    }

    #[cfg(feature="terminal")]
    /// Display the prompt and return the user answer.
    pub fn prompt(&mut self) -> io::Result<String> {
        key_listener::listen(self, false)?;
        Ok(self.get_value().to_owned())
    }
}

impl Password<'_> {
    fn get_value(&self) -> &str {
        match self.input.value.is_empty() {
            true => self.default_value.unwrap_or_default(),
            false => &self.input.value,
        }
    }

    fn validate_to_submit(&mut self) -> bool {
        if let Some(validator) = &self.validator {
            self.validator_result = validator(self.get_value());
        }

        self.validator_result.is_ok()
    }
}

#[cfg(feature="terminal")]
impl Typeable<KeyEvent> for Password<'_> {
    fn handle_key(&mut self, key: &KeyEvent) -> bool {
        let mut submit = false;

        match key.code {
            // submit
            KeyCode::Enter => submit = self.validate_to_submit(),
            // type
            KeyCode::Char(c) => self.input.insert(c),
            // remove delete
            KeyCode::Backspace => self.input.backspace(),
            KeyCode::Delete => self.input.delete(),
            // move cursor
            KeyCode::Left => self.input.move_cursor(Direction::Left),
            KeyCode::Right => self.input.move_cursor(Direction::Right),
            _ => (),
        };

        submit
    }
}

#[cfg(feature="bevy")]
impl Typeable<KeyEvent> for Password<'_> {
    fn handle_key(&mut self, key: &KeyEvent) -> bool {
        let mut submit = false;

        for c in key.chars.iter() {
            if ! c.is_control() {
                self.input.insert(*c);
            }
        }

        for code in &key.codes {
            match code {
                // submit
                KeyCode::Return => submit = self.validate_to_submit(),
                // type
                // KeyCode::Char(c) => self.input.insert(c),
                // remove delete
                KeyCode::Back => self.input.backspace(),
                KeyCode::Delete => self.input.delete(),
                // move cursor
                KeyCode::Left => self.input.move_cursor(Direction::Left),
                KeyCode::Right => self.input.move_cursor(Direction::Right),
                _ => (),
            };
        }

        submit
    }
}
impl Printable for Password<'_> {
    fn draw<R: Renderer>(&self, renderer: &mut R) -> io::Result<()> {
        let mut out = ColoredStrings::default();
        let cursor = (self.formatter)(self, renderer.draw_time(), &mut out);
        renderer.print(out)?;
        renderer.set_cursor(cursor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_placeholder() {
        let mut text = Password::new("");

        assert_eq!(text.placeholder, None);
        text.placeholder("foo");
        assert_eq!(text.placeholder, Some("foo"));
    }

    #[test]
    fn set_default_value() {
        let mut text = Password::new("");

        assert_eq!(text.default_value, None);
        text.default("foo");
        assert_eq!(text.default_value, Some("foo"));
    }

    #[test]
    fn set_initial_value() {
        let mut prompt = Password::new("");

        assert_eq!(prompt.input, LineInput::new());

        prompt.initial("foo");

        assert_eq!(
            prompt.input,
            LineInput {
                value: String::from("foo"),
                col: 3,
            }
        );
    }

    #[test]
    fn set_custom_formatter() {
        let mut prompt: Password = Password::new("");
        let draw_time = DrawTime::First;
        const EXPECTED_VALUE: &str = "foo";

        prompt.format(|_, _| (String::from(EXPECTED_VALUE), [0, 0]));

        assert_eq!(
            (prompt.formatter)(&prompt, draw_time()),
            (String::from(EXPECTED_VALUE), [0, 0])
        );
    }

    #[test]
    fn set_hidden_value() {
        let mut prompt = Password::new("");

        assert!(!prompt.hidden);
        prompt.hidden(true);
        assert!(prompt.hidden)
    }

    #[test]
    fn update_cursor_position() {
        let mut prompt = Password::new("");
        prompt.input.set_value("foo");
        prompt.input.col = 2;

        let keys = [(KeyCode::Left, 1), (KeyCode::Right, 2)];

        for (key, expected) in keys {
            prompt.handle_key(KeyEvent::from(key));

            assert_eq!(prompt.input.col, expected);
        }
    }

    #[test]
    fn submit_input_value() {
        let mut prompt = Password::new("");
        prompt.input.set_value("foo");
        prompt.default("bar");

        assert_eq!(prompt.get_value(), "foo");
    }

    #[test]
    fn submit_default_value() {
        let mut prompt = Password::new("");
        prompt.input.set_value("");
        prompt.default("bar");

        assert_eq!(prompt.get_value(), "bar");
    }
}
