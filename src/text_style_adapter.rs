use std::io;
use std::fmt::{self, Display, Write};
use text_style::{self, bevy::TextStyleParams, AnsiColor, AnsiMode, StyledString, Style, Color};
use crossterm::{cursor, execute, queue, style::{self, Print, SetForegroundColor, SetBackgroundColor}, terminal};
use crate::{DrawTime, utils::renderer::Renderer};

#[derive(Debug, Clone, Default)]
pub struct StyledStringWriter {
    pub style: Option<text_style::Style>,
    pub strings: Vec<StyledString>,
    pub state: RendererState,
}

impl StyledStringWriter {
    pub fn clear(&mut self) {
        self.state = RendererState::default();
    }

}

#[derive(Debug, Default, Clone)]
pub struct RendererState {
    pub(crate) draw_time: DrawTime,
    pub(crate) cursor_visible: bool,
    pub(crate) cursor_pos: [usize; 2],
}

pub trait Command {
    fn write_style(&self, f: &mut StyledStringWriter) -> Result<(), fmt::Error>;
}

impl<T: Display> Command for Print<T> {
    fn write_style(&self, f: &mut StyledStringWriter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl Command for SetForegroundColor{
    fn write_style(&self, f: &mut StyledStringWriter) -> Result<(), fmt::Error> {
        // let mut style = f.style.get_or_insert_default();
        let style = f.style.get_or_insert(Style::default());
        // style.fg = Some(text_style::Color::from(self.0));
        style.fg = Some(from_crossterm(self.0));
        Ok(())
    }
}

impl Command for SetBackgroundColor{
    fn write_style(&self, f: &mut StyledStringWriter) -> Result<(), fmt::Error> {
        // let mut style = f.style.get_or_insert_default();
        let style = f.style.get_or_insert(Style::default());
        // style.fg = Some(text_style::Color::from(self.0));
        style.bg = Some(from_crossterm(self.0));
        Ok(())
    }
}
// impl From<Color> for style::Color {
    fn from_crossterm(color: style::Color) -> text_style::Color {
        use AnsiColor::*;
        use AnsiMode::*;

        match color {
            style::Color::Rgb { r, g, b } => Color::Rgb { r, g, b },
            x => {
                let (mode, color) = match x {
                    style::Color::Black => (Dark, Black),
                    style::Color::Black => (Dark, Black),
                    style::Color::DarkRed => (Dark, Red),
                    style::Color::DarkGreen => (Dark, Green),
                    style::Color::DarkYellow => (Dark, Yellow),
                    style::Color::DarkBlue => (Dark, Blue),
                    style::Color::DarkMagenta => (Dark, Magenta),
                    style::Color::DarkCyan => (Dark, Cyan),
                    // TODO: check greys
                    style::Color::Grey => (Dark, White),
                    style::Color::DarkGrey => (Light, Black),
                    style::Color::Red => (Light, Red),
                    style::Color::Green => (Light, Green),
                    style::Color::Yellow => (Light, Yellow),
                    style::Color::Blue => (Light, Blue),
                    style::Color::Magenta => (Light, Magenta),
                    style::Color::Cyan => (Light, Cyan),
                    style::Color::White => (Light, White),
                    _ => todo!(),
                };
                Color::Ansi { color, mode }
            },
            // style::Color::Black => (Dark, Black),
            // Color::Ansi { color, mode } => match (mode, color) {
            //     (Dark, Black) => style::Color::Black,
            // },
            // Color::Rgb { r, g, b } => style::Color::Rgb { r, g, b },
        }
    }
// }

impl std::io::Write for StyledStringWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let s = std::str::from_utf8(buf).expect("Not a utf8 string");
        let ss = match self.strings.pop() {
            None => StyledString::new(s.to_string(), self.style.clone()),
            Some(mut text) => {
                if text.style == self.style {
                    text.s.push_str(s);
                    text
                } else {
                    self.strings.push(text);
                    StyledString::new(s.to_string(),
                                      self.style.clone())
                }
            }
        };
        self.strings.push(ss);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}


impl std::fmt::Write for StyledStringWriter {
    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        // let s = std::str::from_utf8(buf).expect("Not a utf8 string");
        let ss = match self.strings.pop() {
            None => StyledString::new(s.to_string(), self.style.clone()),
            Some(mut text) => {
                if text.style == self.style {
                    text.s.push_str(s);
                    text
                } else {
                    self.strings.push(text);
                    StyledString::new(s.to_string(),
                                      self.style.clone())
                }
            }
        };
        self.strings.push(ss);
        Ok(())
    }
}

impl Renderer for StyledStringWriter {
    type Writer = StyledStringWriter;
    fn draw_time(&self) -> DrawTime {
        self.state.draw_time
    }

    fn update_draw_time(&mut self) {
        self.state.draw_time = match self.state.draw_time {
            DrawTime::First => DrawTime::Update,
            _ => DrawTime::Last,
        }
    }

    fn set_foreground(&mut self, color: Color) -> io::Result<()> {
        let style = self.style.get_or_insert(Style::default());
        style.fg = Some(color);
        Ok(())
    }

    fn set_background(&mut self, color: Color) -> io::Result<()> {
        let style = self.style.get_or_insert(Style::default());
        style.bg = Some(color);
        Ok(())
    }

    fn reset_color(&mut self) -> io::Result<()> {
        let mut style = self.style.get_or_insert(Style::default());
        style.fg = None;
        style.bg = None;
        Ok(())
    }

    // fn print(&mut self, strings: ColoredStrings) -> io::Result<()> {
    fn print2<F>(&mut self, draw_text: F) -> io::Result<()>
    where
        F: FnOnce(&mut Self::Writer) -> io::Result<u16> {
        Ok(())
    }

    fn print_prompt<F>(&mut self, draw_prompt: F) -> io::Result<()>
    where
        F: FnOnce(&mut Self) -> io::Result<u16> {
        let text_lines = draw_prompt(self)? - 1;
        Ok(())
    }

    /// Utility function for line input.
    /// Set initial position based on the position after drawing.
    fn set_cursor(&mut self, [x, y]: [usize; 2]) -> io::Result<()> {
        if self.state.draw_time == DrawTime::Last {
            return Ok(());
        }
        self.state.cursor_pos[0] = x;
        self.state.cursor_pos[1] = y;
        Ok(())
    }

    fn hide_cursor(&mut self) -> io::Result<()> {
        self.state.cursor_visible = false;
        Ok(())
    }

    fn show_cursor(&mut self) -> io::Result<()> {
        self.state.cursor_visible = true;
        Ok(())
    }
}
// impl QueueableCommand for StyledStringWriter {
//     fn queue(&mut self, command: impl Command) -> Result<&mut Self> {


//     }
// }
