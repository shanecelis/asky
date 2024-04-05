//! Style for asky prompts
use crate::{utils::renderer::Renderer, Typeable, Valuable};
use bitflags::bitflags;
use std::io;
use text_style::AnsiColor::*;

bitflags! {
    /// [Section::Option] and [Section::OptionExclusive] flags
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Flags: u8 {
        /// Focused
        const Focused  = 0b00000001;
        /// Selected
        const Selected = 0b00000010;
        /// Disabled
        const Disabled = 0b00000100;
    }
}

/// Section of render, emitted before and after the content of each item.
#[derive(Clone, Copy, Debug)]
pub enum Section {
    /// If query has been answered, `Query(true)`
    Query(bool),
    /// Show answer if `Answer(true)`.
    Answer(bool),
    /// The default answer if any
    DefaultAnswer,
    /// A message
    Message,
    /// Call to action for a message
    Action,
    /// If toggled, `Toggle(true)`
    Toggle(bool),
    /// Option (or checkbox) with [Flags]
    Option(Flags),
    /// Exclusive option (or radiobox) with [Flags]
    OptionExclusive(Flags),
    /// List
    List,
    /// If this is the first list item, `ListItem(true)`.
    ListItem(bool), // if first -> ListItem(true)
    // Cursor,
    /// Placeholder is shown when input is empty.
    Placeholder,
    /// If valid, `Validator(true)`.
    Validator(bool), // if valid -> Validator(true)
    /// Input
    Input,
    /// Page, e.g., Page 0 of 8 is represented by `Page(0, 8)`.
    Page(u8, u8), // Page 0 of 8 -> Page(0, 8)
    /// A custom section
    #[allow(dead_code)]
    Custom(&'static str),
}

/// Given a prompt `T` this will use style `S` as its new default style.
pub struct WithStyle<T, S>(pub(crate) T, pub(crate) S);

#[cfg(feature = "bevy")]
impl<T, S> Typeable<crate::bevy::KeyEvent> for WithStyle<T, S>
where
    T: Typeable<crate::bevy::KeyEvent>,
{
    fn handle_key(&mut self, key: &crate::bevy::KeyEvent) -> bool {
        self.0.handle_key(key)
    }
}

#[cfg(feature = "terminal")]
impl<T, S> Typeable<crossterm::event::KeyEvent> for WithStyle<T, S>
where
    T: Typeable<crossterm::event::KeyEvent>,
{
    fn handle_key(&mut self, key: &crossterm::event::KeyEvent) -> bool {
        self.0.handle_key(key)
    }
}

impl<T, S> Valuable for WithStyle<T, S>
where
    T: Valuable,
{
    type Output = T::Output;
    fn value(&self) -> Result<Self::Output, crate::Error> {
        self.0.value()
    }
}

impl<T, S> Valuable for WithFormat<T, S>
where
    T: Valuable,
{
    type Output = T::Output;
    fn value(&self) -> Result<Self::Output, crate::Error> {
        self.0.value()
    }
}

/// Given a prompt `T` this will use format `F` as its new default format.
pub struct WithFormat<T, F>(pub(crate) T, pub(crate) F);


#[cfg(feature = "bevy")]
impl<T, S> Typeable<crate::bevy::KeyEvent> for WithFormat<T, S>
where
    T: Typeable<crate::bevy::KeyEvent>,
{
    fn handle_key(&mut self, key: &crate::bevy::KeyEvent) -> bool {
        self.0.handle_key(key)
    }
}

#[cfg(feature = "terminal")]
impl<T, S> Typeable<crossterm::event::KeyEvent> for WithFormat<T, S>
where
    T: Typeable<crossterm::event::KeyEvent>,
{
    fn handle_key(&mut self, key: &crossterm::event::KeyEvent) -> bool {
        self.0.handle_key(key)
    }
}

// impl<T, S> Valuable for WithStyle<T, S>
// impl<T,S> std::ops::Deref for WithFormat<T,S> {
//     type Target = T;

//     // Required method
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }

/// A style is passed the beginning and end of [Section]s while rendering.
pub trait Style {
    /// A style [Section] begins. Prepare, e.g., set foreground color.
    fn begin(&self, renderer: &mut dyn Renderer, section: Section) -> io::Result<()>;
    /// A style [Section] ends. Finish, e.g., reset colors.
    fn end(&self, renderer: &mut dyn Renderer, section: Section) -> io::Result<()>;
}

/// No style; do nothing. Useful for testing.
pub struct NoStyle;

impl Style for NoStyle {
    fn begin(&self, _renderer: &mut dyn Renderer, _section: Section) -> io::Result<()> {
        Ok(())
    }
    fn end(&self, _renderer: &mut dyn Renderer, _section: Section) -> io::Result<()> {
        Ok(())
    }
}

impl Style for DefaultStyle {
    fn begin(&self, r: &mut dyn Renderer, section: Section) -> io::Result<()> {
        use Section::*;
        match section {
            Query(answered) => {
                if answered {
                    r.set_foreground(Green.dark())?;
                    write!(r, "{}", if self.ascii { "[x]" } else { "■" })?;
                    r.reset_color()?;
                    write!(r, " ")?;
                } else {
                    r.set_foreground(Blue.dark())?;
                    write!(r, "{}", if self.ascii { "[ ]" } else { "▣" })?;
                    r.reset_color()?;
                    write!(r, " ")?;
                }
            }
            Answer(show) => {
                r.set_foreground(Magenta.dark())?;
                if !show {
                    write!(r, "{}", if self.ascii { "..." } else { "…" })?;
                }
            } // was purple
            Toggle(selected) => {
                if selected {
                    r.set_foreground(Black.dark())?;
                    r.set_background(Blue.dark())?;
                    write!(r, " ")?;
                } else {
                    r.set_foreground(White.dark())?;
                    r.set_background(Black.light())?;
                    write!(r, " ")?;
                }
            }
            OptionExclusive(flags) => {
                match (
                    flags.contains(Flags::Focused),
                    flags.contains(Flags::Disabled),
                ) {
                    (false, _) => {
                        r.set_foreground(Black.light())?;
                        write!(r, "{}", if self.ascii { "( )" } else { "○" })?;
                        r.reset_color()?;
                    }
                    (true, true) => {
                        r.set_foreground(Red.dark())?;
                        write!(r, "{}", if self.ascii { "( )" } else { "○" })?;
                        r.reset_color()?;
                    }
                    (true, false) => {
                        r.set_foreground(Blue.dark())?;
                        write!(r, "{}", if self.ascii { "(x)" } else { "●" })?;
                        r.reset_color()?;
                    }
                }
                write!(r, " ")?;
                match (
                    flags.contains(Flags::Focused),
                    flags.contains(Flags::Disabled),
                ) {
                    (_, true) => {
                        r.set_foreground(Black.light())?;
                        // SetAttribute(Attribute::OverLined).write_ansi(f)?;
                    }
                    (true, false) => {
                        r.set_foreground(Blue.dark())?;
                    }
                    (false, false) => {}
                }
            }
            Option(flags) => {
                let prefix = match (
                    flags.contains(Flags::Selected),
                    flags.contains(Flags::Focused),
                ) {
                    (true, true) => {
                        if self.ascii {
                            "[o]"
                        } else {
                            "▣"
                        }
                    }
                    (true, false) => {
                        if self.ascii {
                            "[x]"
                        } else {
                            "■"
                        }
                    }
                    _ => {
                        if self.ascii {
                            "[ ]"
                        } else {
                            "□"
                        }
                    }
                };

                match (
                    flags.contains(Flags::Focused),
                    flags.contains(Flags::Selected),
                    flags.contains(Flags::Disabled),
                ) {
                    (true, _, true) => r.set_foreground(Red.dark())?,
                    (true, _, false) => r.set_foreground(Blue.dark())?,
                    (false, true, _) => r.reset_color()?,
                    (false, false, _) => r.set_foreground(Black.light())?,
                };
                write!(r, "{}", prefix)?;
                r.reset_color()?;
                write!(r, " ")?;
                match (
                    flags.contains(Flags::Focused),
                    flags.contains(Flags::Disabled),
                ) {
                    (_, true) => {
                        r.set_foreground(Black.light())?;
                        // SetAttribute(Attribute::OverLined).write_ansi(f)?;
                    }
                    (true, false) => {
                        r.set_foreground(Blue.dark())?;
                    }
                    (false, false) => {}
                }
            }
            Message => {}
            Action => {
                write!(r, " ")?;
                r.set_foreground(Blue.dark())?;
            }
            Validator(valid) => {
                r.set_foreground(if valid { Blue.dark() } else { Red.dark() })?;
            }
            Placeholder => {
                r.set_foreground(Black.light())?;
                write!(r, "Default: ")?;
            }
            Input => {
                r.set_foreground(Blue.dark())?;
                write!(r, "{}", if self.ascii { ">" } else { "›" })?;
                r.reset_color()?;
                write!(r, " ")?;
            }
            List => write!(r, "[")?,
            ListItem(first) => {
                if !first {
                    write!(r, ", ")?;
                }
            }
            Page(i, count) => {
                if count != 1 {
                    let icon = if self.ascii { "*" } else { "•" };
                    writeln!(r)?;
                    write!(r, "{}", " ".repeat(if self.ascii { 4 } else { 2 }))?;
                    r.set_foreground(Black.light())?;
                    write!(r, "{}", icon.repeat(i as usize))?;
                    r.reset_color()?;
                    write!(r, "{}", icon)?;
                    r.set_foreground(Black.light())?;
                    write!(r, "{}", icon.repeat(count.saturating_sub(i + 1) as usize))?;
                    r.reset_color()?;
                    writeln!(r)?;
                }
            }
            x => todo!("{:?} not impl", x),
            // x => {},
        }
        Ok(())
    }
    fn end(&self, r: &mut dyn Renderer, section: Section) -> io::Result<()> {
        use Section::*;
        match section {
            Query(answered) => {
                if answered {
                    write!(r, " ")?;
                } else if self.newlines {
                    writeln!(r)?;
                }
            }
            Answer(_) => {
                r.reset_color()?;
                if self.newlines {
                    writeln!(r)?;
                }
            }
            Toggle(_) => {
                write!(r, " ")?;
                r.reset_color()?;
                write!(r, "  ")?;
            }
            OptionExclusive(_flags) | Option(_flags) => {
                writeln!(r)?;
                r.reset_color()?;
            }
            List => write!(r, "]")?,
            ListItem(_) => {}
            Message => writeln!(r)?,
            _ => r.reset_color()?,
        }
        Ok(())
    }
}

/// Default style
#[derive(Clone, Copy, Debug)]
pub struct DefaultStyle {
    /// Use ascii if true.
    pub ascii: bool,
    /// Use newlines if true.
    pub newlines: bool,
}

/// Default style is true for ascii and false for newlines.
impl Default for DefaultStyle {
    fn default() -> Self {
        Self {
            ascii: true,
            newlines: false,
        }
    }
}
