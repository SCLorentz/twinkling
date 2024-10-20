use std::str::Chars;

#[derive(Debug)]
struct Paragraph {
    content: String,
}

fn begin_tag_struct(mut value: Chars<'_>, tag: char) -> Result<Chars<'_>, String>
{
    if value.next() == Some('<') && value.next() == Some(tag) && value.next() == Some('>')
    {
        return Ok(value)
    }
    else
    {
        return Err("tag not found!".to_string())
    }
}

fn end_tag_struct(mut value: Chars<'_>, tag: char) -> Result<Chars<'_>, String>
{
    if value.next() == Some('<') && value.next() == Some('/') && value.next() == Some(tag) && value.next() == Some('>') {
        return Ok(value)
    } else {
        return Err("end of tag not found!".to_string())
    }
}

fn parse_p_tag(input: &str) -> Result<(Paragraph, &str), String>
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

    Ok((Paragraph { content: content.clone() }, chars.as_str()))
}

fn main()
{
    let html = "<p>Olá, mundo!</p><a>ok</a>";
    let result = parse_p_tag(html);

    match result {
        Ok((paragraph, m)) => println!("{:#?} {:?}", paragraph.content, m),
        Err(error) => println!("Erro: {}", error),
    }
}