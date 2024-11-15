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

fn expect(r#char: char, mut value: Chars<'_>) -> Result<Chars<'_>, String>
{
    if value.next() == Some(r#char)
    {
        return Ok(value);
    }
    Err(format!("missing args: `{}`", r#char).to_string())
}

// Todo: make this better, the process is not complete
fn begin_tag_struct(mut value: Chars<'_>) -> Result<(Chars<'_>, Result<char, &str>, Option<String>), String>
{
    let mut bare_text = String::new();
    //
    while let Some(v) = value.next()
    {
        if v == '<' { break }
        bare_text.push_str(&v.to_string())
    }
    //
    let return_bare_text = match bare_text.len()
    {
        0 => None,
        _ => Some(bare_text)
    };
    //
    let tag = match value.next()
    {
        Some(c) if c != '>' => Ok(c),
        _ => return Err("invalid html structure. No element tag found!".to_string()) // Todo: add the pos of the error here
    };

    value = expect('>', value)?;
    //
    Ok((value, tag, return_bare_text))
}

fn end_tag_struct(mut value: Chars<'_>, tag: char) -> Result<Chars<'_>, String>
{
    value = expect('<', value)?;
    value = expect('/', value)?;
    value = expect(tag, value)?;
    value = expect('>', value)?;
    Ok(value)
}

// maybe, if we use a recursive call, we could verify the existence of other html elements inside each other
fn expect_text<'a>(mut chars: Chars<'a>, content: &'a mut String, tag: char) -> Result<(Chars<'a>, String, char), String>
{
    while let Some(this) = chars.next()
    {
        // thanks gemini! I was stuck on that bug
        content.push_str(&this.to_string());

        if let Ok(remaining_chars) = end_tag_struct(chars.to_owned(), tag)
        {
            chars = remaining_chars;
            break;
        }
    }
    //
    Ok((chars, content.to_string(), tag))
}

pub fn parse_html(input: &str) -> Result<Vec<HtmlNode>, String>
{
    let mut content = String::new();
    // Todo: expect bare text
    let (chars, tag, bare_text) = begin_tag_struct(input.chars())?;
    //
    let mut val: Vec<HtmlNode> = Vec::new();
    //
    match bare_text
    {
        Some(b) => val.push(HtmlNode::Text(b)),
        None => ()
    }
    //
    let (chars, content, tag) = expect_text(chars, &mut content, tag?)?;
    // Todo: parse the content inside the text struct
    //println!("?> {:?} -> {:?}", content, parse_html(&content.clone().as_str()));
    //
    let element = HtmlNode::Element
    {
        tag: String::from(tag),
        attributes: None,
        children: vec![HtmlNode::Text(content.to_string())],
    };
    //
    val.push(element.clone());
    //
    if chars.as_str().len() > 0
    {
        let mut b = parse_html(chars.as_str())?;
        val.append(&mut b)
    }

    return Ok(val)
}