use std::io;
use std::borrow::Cow;

use crate::utils::renderer::{DrawTime, Printable, Renderer};
use crate::Error;
use crate::Valuable;
use crate::style::{Section, Style};

// type Formatter<'a> = dyn Fn(&Confirm, DrawTime, &mut ColoredStrings) + 'a + Send + Sync;

/// Prompt to ask yes/no questions.
///
/// # Key Events
///
/// | Key                  | Action                       |
/// | -------------------- | ---------------------------- |
/// | `Enter`, `Backspace` | Submit current/initial value |
/// | `y`, `Y`             | Submit `true`                |
/// | `n`, `N`             | Submit `false`               |
/// | `Left`, `h`, `H`     | Focus `false`                |
/// | `Right`, `l`, `L`    | Focus `true`                 |
///
/// # Examples
///
/// ```no_run
/// use asky::prelude::*;
///
/// # fn main() -> Result<(), Error> {
/// # #[cfg(feature = "terminal")]
/// if Confirm::new("Do you like the pizza?").prompt()? {
///     println!("Great!");
/// } else {
///     println!("Interesting!");
/// }
/// # Ok(())
/// # }
/// ```
// #[derive(Debug)]
pub struct Confirm<'a> {
    /// Message used to display in the prompt.
    pub message: Cow<'a, str>,
    /// Current state of the prompt.
    pub active: bool,
}

impl<'a> Confirm<'a> {
    /// Create a new confirm prompt.
    pub fn new<T: Into<Cow<'a, str>>>(message: T) -> Self {
        Confirm {
            message: message.into(),
            active: false,
        }
    }

    /// Set whether the prompt should be active at start.
    pub fn initial(&mut self, active: bool) -> &mut Self {
        self.active = active;
        self
    }
}

impl Confirm<'_> {
    pub(crate) fn update_and_submit(&mut self, active: bool) -> bool {
        self.active = active;
        true
    }
}

impl Valuable for Confirm<'_> {
    type Output = bool;
    fn value(&self) -> Result<bool, Error> {
        Ok(self.active)
    }
}

impl Printable for Confirm<'_> {
    fn draw_with_style<R: Renderer, S: Style>(&self, r: &mut R, style: &S) -> io::Result<()> {
        use Section::*;
        let draw_time = r.draw_time();
        // let style = DefaultStyle { ascii: true };

        let options = ["No", "Yes"];

        r.pre_prompt()?;
        let line_count: u16 = if draw_time == DrawTime::Last {
            style.begin(r, Query(true))?;
            write!(r, "{}", self.message)?;
            style.end(r, Query(true))?;

            style.begin(r, Answer(true))?;
            write!(r, "{}", options[self.active as usize])?;
            style.end(r, Answer(true))?;
            1
        } else {
            style.begin(r, Query(false))?;
            write!(r, "{}", self.message)?;
            style.end(r, Query(false))?;

            style.begin(r, Toggle(!self.active))?;
            write!(r, "{}", options[0])?;
            style.end(r, Toggle(!self.active))?;
            style.begin(r, Toggle(self.active))?;
            write!(r, "{}", options[1])?;
            style.end(r, Toggle(self.active))?;
            2
        };

        // assert_eq!(r.newline_count(), &line_count);
        r.post_prompt()
    }
}

#[cfg(feature = "terminal")]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::key_listener::Typeable;
    use crossterm::event::{KeyCode, KeyEvent};

    #[test]
    fn set_initial_value() {
        let mut prompt = Confirm::new("");

        prompt.initial(false);
        assert!(!prompt.active);
        prompt.initial(true);
        assert!(prompt.active);
    }

    // #[test]
    // fn set_custom_formatter() {
    //     let mut prompt: Confirm = Confirm::new("");
    //     let draw_time = DrawTime::First;
    //     const EXPECTED_VALUE: &str = "foo";

    //     prompt.format(|_, _, out| out.push(EXPECTED_VALUE.into()));
    //     let mut out = ColoredStrings::new();
    //     (prompt.formatter)(&prompt, draw_time, &mut out);
    //     assert_eq!(format!("{}", out), EXPECTED_VALUE);
    // }

    #[test]
    fn update_and_submit() {
        let events = [('y', true), ('Y', true), ('n', false), ('N', false)];

        for (char, expected) in events {
            let mut prompt = Confirm::new("");
            let simulated_key = KeyEvent::from(KeyCode::Char(char));

            prompt.initial(!expected);
            let submit = prompt.handle_key(&simulated_key);

            assert_eq!(prompt.active, expected);
            assert!(submit);
        }
    }

    #[test]
    fn submit_focused() {
        let events = [KeyCode::Enter, KeyCode::Backspace];

        for event in events {
            let mut prompt = Confirm::new("");
            let simulated_key = KeyEvent::from(event);

            let submit = prompt.handle_key(&simulated_key);
            assert!(!prompt.active);
            assert!(submit);
        }
    }

    #[test]
    fn update_focused() {
        let events = [
            (KeyCode::Left, true, false),
            (KeyCode::Char('h'), true, false),
            (KeyCode::Char('H'), true, false),
            (KeyCode::Right, false, true),
            (KeyCode::Char('l'), false, true),
            (KeyCode::Char('L'), false, true),
        ];

        for (key, initial, expected) in events {
            let mut prompt = Confirm::new("");
            let simulated_key = KeyEvent::from(key);

            prompt.initial(initial);
            let submit = prompt.handle_key(&simulated_key);

            assert_eq!(prompt.active, expected);
            assert!(!submit);
        }
    }
}
