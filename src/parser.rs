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
            let rest = &input[1..];
            let (prefix, suffix) = parse_text(rest);
            (c.to_string() + &prefix, suffix)
        },
        None => (String::new(), input),
    }
}

fn expect(r#char: char, mut value: Chars<'_>) -> Result<Chars<'_>, String>
{
    if value.next() == Some(r#char)
    {
        return Ok(value);
    }
    Err(format!("missing args: `{}`", r#char).to_string())
}

fn parse_tag_struct(mut input: Chars<'_>) -> Result<(Option<char>, &str), String>
{
    input = expect('<', input)?;
    //
    let tag = input.next();
    //
    let input = expect('>', input)?;
    //
    Ok((tag, input.as_str()))
}

pub fn parse_html(input: &str) -> Result<Vec<HtmlNode>, String>
{
    let (bare_text, input) = parse_text(input);
    //
    let (_, input) = parse_tag_struct(input.chars())?;
    //
    let mut result = Vec::new();
    result.push(HtmlNode::Text(bare_text));
    result.push(HtmlNode::Text(input.to_string()));

    Ok(result)
}