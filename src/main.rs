extern crate gtk;
use std::str::Chars;

use gtk::prelude::*;
use gtk::{Application, ApplicationWindow};

mod ui;
use ui::*;

fn build_ui() {
    let app = Application::builder()
        .application_id("org.example.HelloWorld")
        .build();

    app.connect_activate(|app| {
        // We create the main window.
        let win = ApplicationWindow::builder()
            .application(app)
            .default_width(320)
            .default_height(200)
            .title("Hello, World!")
            .build();

        // Don't forget to make all widgets visible.
        win.show_all();
    });

    app.run();
}

#[derive(Debug)]
enum HtmlNode {
    Text(String),
    Element {
        #[allow(unused)]
        tag: String,
        #[allow(unused)]
        attributes: Option<Vec<(String, String)>>,
        children: Vec<HtmlNode>,
    },
}

fn expect_value(r#char: char, mut value: Chars<'_>) -> Result<Chars<'_>, String>
{
    if value.next() == Some(r#char) {
        Ok(value)
    } else {
        Err(format!("missing args: `{}`", r#char).to_string())
    }
}

fn begin_tag_struct(mut value: Chars<'_>, tag: char) -> Result<Chars<'_>, String>
{
    value = expect_value('<', value)?;
    value = expect_value(tag, value)?;
    value = expect_value('>', value)?;
    Ok(value)
}

fn end_tag_struct(mut value: Chars<'_>, tag: char) -> Result<Chars<'_>, String>
{
    value = expect_value('<', value)?;
    value = expect_value('/', value)?;
    value = expect_value(tag, value)?;
    value = expect_value('>', value)?;
    Ok(value)
}

fn parse_p_tag(input: &str) -> Result<(HtmlNode, &str), String>
{
    let (mut chars, mut content) = (input.chars(), String::new());
    //
    chars = begin_tag_struct(chars.to_owned(), 'p')?;
    
    while let Some(char) = chars.next()
    {
        let val = end_tag_struct(chars.to_owned(), 'p');
        //
        if val.is_ok() {
            chars = val?;
            break
        }
        //
        content.push_str(&char.to_string());
    }

    let return_value = HtmlNode::Element {
        tag: String::from("p"),
        attributes: None,
        children: vec![HtmlNode::Text(content)],
    };

    return Ok((return_value, chars.as_str()))
}

fn extract_text(node: &HtmlNode) -> String {
    match node {
        HtmlNode::Text(text) => text.to_owned(),
        HtmlNode::Element { children, .. } => {
            let mut result = String::new();
            for child in children {
                result += &extract_text(child);
            }
            result
        }
    }
}

fn main()
{
    let html = "<p>Olá, mundo!</p><a>ok</a>";
    let result = parse_p_tag(html);

    match result {
        Ok((html, m)) => println!("text: {:#?}, tag: unknow, remain: {:?}", extract_text(&html), m),
        Err(error) => println!("Erro: {}", error),
    }

    build_ui();
}