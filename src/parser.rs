use std::str::Chars;

#[derive(Debug, Clone)]
pub enum HtmlNode
{
    Text(String),
    Element
    {
        tag: String,
        attributes: Option<Vec<(String, String)>>,
        children: Vec<HtmlNode>,
    },
}

fn parse_text(input: &str) -> (String, &str)
{
    match input.chars().next() 
    {
        Some('<') => (String::new(), input),
        Some(c) =>
        {
            let (prefix, suffix) = parse_text(&input[1..]);
            (format!("{}{}", c, prefix), suffix)
        },
        None => (String::new(), input),
    }
}

fn expect(expected: char, value: Chars<'_>) -> Result<Chars<'_>, String>
{
    value.clone().next()
        .filter(|&c| c == expected)
        .map(|_| value)
        .ok_or_else(|| format!("missing args: `{}`", expected))
}

fn parse_tag_struct(input: Chars<'_>) -> Result<(Option<char>, &str), String>
{
    expect('<', input)
        .and_then(|chars| 
        {
            let tag = chars.clone().next();
            expect('>', chars).map(|chars| (tag, chars.as_str()))
        })
}

pub fn parse_html(input: &str) -> Result<Vec<HtmlNode>, String>
{
    let (bare_text, remaining) = parse_text(input);
    parse_tag_struct(remaining.chars()).map(|(_, content)| 
    {
        vec![
            HtmlNode::Text(bare_text),
            HtmlNode::Text(content.to_string()),
        ]
    })
}