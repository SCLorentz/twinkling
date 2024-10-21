use std::str::Chars;

#[derive(Debug)]
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
    if value.next() == Some(r#char) {
        Ok(value)
    } else {
        Err(format!("missing args: `{}`", r#char).to_string())
    }
}

fn begin_tag_struct(mut value: Chars<'_>) -> Result<(Chars<'_>, Result<char, &str>), String>
{
    value = expect('<', value)?;
    //value = expect(tag, value)?;
    let tag = match value.next()
    {
        Some(c) if c != '>' => Ok(c),
        _ => return Err("invalid html structure. No element tag found!".to_string())
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

pub fn parse_html(input: &str) -> Result<(HtmlNode, &str), String>
{
    let mut content = String::new();
    let (mut chars, tag ) = begin_tag_struct(input.chars())?;
    //
    
    while let Some(this) = chars.next()
    {
        let val = end_tag_struct(chars.to_owned(), tag?);
        //
        if val.is_ok()
        {
            chars = val?;
            break
        }
        //
        content.push_str(&this.to_string());
    }

    let return_value = HtmlNode::Element {
        tag: String::from(tag?),
        attributes: None,
        children: vec![HtmlNode::Text(content)],
    };

    return Ok((return_value, chars.as_str()))
}