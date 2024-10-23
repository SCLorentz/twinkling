use std::str::Chars;

#[derive(Debug, Clone)]
pub enum HtmlNode {
    Text(String),
    Element {
        tag: String,
        attributes: Option<Vec<(String, String)>>,
        children: Vec<HtmlNode>,
    },
}

fn expect(r#char: char, mut value: Chars<'_>) -> Result<Chars<'_>, String>
{
    if value.next() == Some(r#char)
    {
        return Ok(value);
    }
    Err(format!("missing args: `{}`", r#char).to_string())
}

fn begin_tag_struct(mut value: Chars<'_>) -> Result<(Chars<'_>, Result<char, &str>), String>
{
    value = expect('<', value)?;
    //
    let tag = match value.next()
    {
        Some(c) if c != '>' => Ok(c),
        _ => return Err("invalid html structure. No element tag found!".to_string()) // Todo: add the pos of the error here
    };

    value = expect('>', value)?;
    Ok((value, tag))
}

fn end_tag_struct(mut value: Chars<'_>, tag: char) -> Result<Chars<'_>, String>
{
    value = expect('<', value)?;
    value = expect('/', value)?;
    value = expect(tag, value)?;
    value = expect('>', value)?;
    Ok(value)
}

pub fn parse_html(input: &str) -> Result<Vec<HtmlNode>, String>
{
    let mut content = String::new();
    let (mut chars, tag ) = begin_tag_struct(input.chars())?;
    //
    while let Some(this) = chars.next()
    {
        // thanks gemini! I was stuck on that bug
        content.push_str(&this.to_string());

        if let Ok(remaining_chars) = end_tag_struct(chars.to_owned(), tag?) {
            chars = remaining_chars;
            break;
        }
    }

    let element = HtmlNode::Element {
        tag: String::from(tag?),
        attributes: None,
        children: vec![HtmlNode::Text(content)],
    };

    let mut val: Vec<HtmlNode> = Vec::new();
    val.push(element.clone());
    //
    if chars.as_str().len() > 0 {
        let mut b = parse_html(chars.as_str())?;
        val.append(&mut b)
    }

    return Ok(val)
}