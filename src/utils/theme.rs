use colored::Colorize;

use crate::prompts::{
    confirm::Confirm,
    multi_select::MultiSelect,
    number::Number,
    password::Password,
    select::{Select, SelectCursor, SelectOption},
    text::Text,
    toggle::Toggle,
};

use super::{
    num::Num,
    renderer::{DrawTime, Renderer},
};

const LINE_CURSOR: Option<(u16, u16)> = Some((1, 2));

pub fn fmt_confirm(prompt: &Confirm, renderer: &Renderer) -> String {
    if renderer.draw_time == DrawTime::Last {
        return fmt_last_message(prompt.message, if prompt.active { "Yes" } else { "No" });
    }

    [
        fmt_message(prompt.message),
        fmt_toggle_options(("No", "Yes"), prompt.active),
        String::new(),
    ]
    .join("\n")
}

pub fn fmt_toggle(prompt: &Toggle, renderer: &Renderer) -> String {
    if renderer.draw_time == DrawTime::Last {
        return fmt_last_message(
            prompt.message,
            if prompt.active {
                prompt.options.1
            } else {
                prompt.options.0
            },
        );
    }

    [
        fmt_message(prompt.message),
        fmt_toggle_options(prompt.options, prompt.active),
        String::new(),
    ]
    .join("\n")
}

pub fn fmt_select<T>(prompt: &Select<T>, renderer: &Renderer) -> String {
    if renderer.draw_time == DrawTime::Last {
        return fmt_last_message(prompt.message, prompt.options[prompt.cursor.focused].title);
    }

    [
        fmt_message(prompt.message),
        fmt_select_page_options(&prompt.options, &prompt.cursor, false),
        fmt_select_pagination(prompt.cursor.get_page(), prompt.cursor.count_pages()),
    ]
    .join("\n")
}

pub fn fmt_multi_select<T>(prompt: &MultiSelect<T>, renderer: &Renderer) -> String {
    if renderer.draw_time == DrawTime::Last {
        return fmt_last_message(
            prompt.message,
            &format!(
                "[{}]",
                prompt
                    .options
                    .iter()
                    .filter(|opt| opt.active)
                    .map(|opt| opt.title)
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
        );
    }

    [
        fmt_multi_select_message(prompt.message, prompt.min, prompt.max),
        fmt_select_page_options(&prompt.options, &prompt.cursor, true),
        fmt_select_pagination(prompt.cursor.get_page(), prompt.cursor.count_pages()),
    ]
    .join("\n")
}

pub fn fmt_text(prompt: &Text, renderer: &Renderer) -> (String, Option<(u16, u16)>) {
    if renderer.draw_time == DrawTime::Last {
        return (fmt_last_message(prompt.message, &prompt.input.value), None);
    }

    (
        [
            fmt_line_message(prompt.message, &prompt.default_value),
            fmt_line_input(
                &prompt.input.value,
                &prompt.placeholder,
                &prompt.validator_result,
                false,
            ),
            fmt_line_validator(&prompt.validator_result),
        ]
        .join("\n"),
        LINE_CURSOR,
    )
}

pub fn fmt_password(prompt: &Password, renderer: &Renderer) -> (String, Option<(u16, u16)>) {
    if renderer.draw_time == DrawTime::Last {
        return (fmt_last_message(prompt.message, "…"), None);
    }

    let text = match prompt.hidden {
        true => String::new(),
        false => "*".repeat(prompt.input.value.len()),
    };

    (
        [
            fmt_line_message(prompt.message, &prompt.default_value),
            fmt_line_input(&text, &prompt.placeholder, &prompt.validator_result, false),
            fmt_line_validator(&prompt.validator_result),
        ]
        .join("\n"),
        LINE_CURSOR,
    )
}

pub fn fmt_number<T: Num>(prompt: &Number<T>, renderer: &Renderer) -> (String, Option<(u16, u16)>) {
    if renderer.draw_time == DrawTime::Last {
        return (fmt_last_message(prompt.message, &prompt.input.value), None);
    }

    (
        [
            fmt_line_message(prompt.message, &prompt.default_value.as_deref()),
            fmt_line_input(
                &prompt.input.value,
                &prompt.placeholder,
                &prompt.validator_result,
                true,
            ),
            fmt_line_validator(&prompt.validator_result),
        ]
        .join("\n"),
        LINE_CURSOR,
    )
}

// region: general

fn fmt_message(message: &str) -> String {
    format!("{} {}", "▣".blue(), message)
}

fn fmt_last_message(message: &str, answer: &str) -> String {
    format!("{} {} {}\n", "■".green(), message, answer.purple())
}

// endregion: general

// region: toggle

fn fmt_toggle_options(options: (&str, &str), active: bool) -> String {
    let fmt_option = |opt, active| {
        let opt = format!(" {} ", opt);
        match active {
            true => opt.black().on_blue(),
            false => opt.white().on_bright_black(),
        }
    };

    format!(
        "{}  {}",
        fmt_option(options.0, !active),
        fmt_option(options.1, active)
    )
}

// endregion: toggle

// region: line

fn fmt_line_message(msg: &str, default_value: &Option<&str>) -> String {
    let value = match default_value {
        Some(value) => format!("Default: {}", value).bright_black(),
        None => "".normal(),
    };

    format!("{} {}", fmt_message(msg), value)
}

fn fmt_line_input(
    input: &str,
    placeholder: &Option<&str>,
    validator_result: &Result<(), &str>,
    is_number: bool,
) -> String {
    let prefix = match validator_result {
        Ok(_) => "›".blue(),
        Err(_) => "›".red(),
    };

    let input = match (input.is_empty(), is_number) {
        (true, _) => placeholder.unwrap_or_default().bright_black(),
        (false, true) => input.yellow(),
        (false, false) => input.normal(),
    };

    format!("{} {}", prefix, input)
}

fn fmt_line_validator(validator_result: &Result<(), &str>) -> String {
    match validator_result {
        Ok(_) => String::new(),
        Err(e) => format!("{}\n", e.red()),
    }
}

// endregion: line

// region: select

fn fmt_multi_select_message(msg: &str, min: Option<usize>, max: Option<usize>) -> String {
    let min_max = match (min, max) {
        (None, None) => String::new(),
        (None, Some(max)) => format!("Max: {}", max),
        (Some(min), None) => format!("Min: {}", min),
        (Some(min), Some(max)) => format!("Min: {} · Max: {}", min, max),
    }
    .bright_black();

    format!("{} {}", fmt_message(msg), min_max)
}

fn fmt_select_page_options<T>(
    options: &Vec<SelectOption<T>>,
    cursor: &SelectCursor,
    is_multiple: bool,
) -> String {
    let items_per_page = cursor.items_per_page;
    let options_len = options.len();

    let page_len = items_per_page.min(options_len);
    let page_start = cursor.get_page() * items_per_page;
    let page_end = (page_start + page_len).min(options_len);
    let page_focused = cursor.focused % items_per_page;

    let mut page_options: Vec<String> = options[page_start..page_end]
        .iter()
        .enumerate()
        .map(|(i, option)| fmt_select_option(option, page_focused == i, is_multiple))
        .collect();

    page_options.resize(page_len, String::new());
    page_options.join("\n")
}

fn fmt_select_pagination(page: usize, pages: usize) -> String {
    if pages == 1 {
        return String::new();
    }

    let icon = "•";

    format!(
        "\n  {}{}{}\n",
        icon.repeat(page).bright_black(),
        icon,
        icon.repeat(pages.saturating_sub(page + 1)).bright_black(),
    )
}

fn fmt_select_option<T>(option: &SelectOption<T>, focused: bool, multiple: bool) -> String {
    let prefix = if multiple {
        let prefix = match (option.active, focused) {
            (true, true) => "◉",
            (true, false) => "●",
            _ => "○",
        };

        match (focused, option.active, option.disabled) {
            (true, _, true) => prefix.red(),
            (true, _, false) => prefix.blue(),
            (false, true, _) => prefix.normal(),
            (false, false, _) => prefix.bright_black(),
        }
    } else {
        match (focused, option.disabled) {
            (false, _) => "○".bright_black(),
            (true, true) => "○".red(),
            (true, false) => "●".blue(),
        }
    };

    let title = option.title;
    let title = match (option.disabled, focused) {
        (true, _) => title.bright_black().strikethrough(),
        (false, true) => title.blue(),
        (false, false) => title.normal(),
    };

    let make_description = |s: &str| format!(" · {}", s).bright_black();
    let description = match (focused, option.disabled, option.description) {
        (true, true, _) => make_description("(Disabled)"),
        (true, false, Some(description)) => make_description(description),
        _ => "".normal(),
    };

    format!("{} {} {}", prefix, title, description)
}

// endregion: select
