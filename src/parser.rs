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

fn begin_tag_struct(mut value: Chars<'_>, tag: char) -> Result<Chars<'_>, String>
{
    value = expect('<', value)?;
    value = expect(tag, value)?;
    value = expect('>', value)?;
    Ok(value)
}

fn end_tag_struct(mut value: Chars<'_>, tag: char) -> Result<Chars<'_>, String>
{
    value = expect('<', value)?;
    value = expect('/', value)?;
    value = expect(tag, value)?;
    value = expect('>', value)?;
    Ok(value)
}

pub fn parse_p_tag(input: &str) -> Result<(HtmlNode, &str), String>
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