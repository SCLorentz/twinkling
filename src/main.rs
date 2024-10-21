use std::str::Chars;


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

fn extract_text(node: &HtmlNode) -> String
{
    match node
    {
        HtmlNode::Text(text) => text.to_owned(),
        HtmlNode::Element { children, .. } =>
        {
            let mut result = String::new();
            for child in children
            {
                result = extract_text(child);
            }
            result
        }
    }
}

fn extract_html_values(node: &HtmlNode) -> Result<(&String, &Option<Vec<(String, String)>>, &Vec<HtmlNode>), String>
{
    match node
    {
        HtmlNode::Element { tag, attributes, children } =>
        {
            return Ok((tag, attributes, children))
        }
        _ => return Err("not a node element!".to_string())
    }
}

fn main()
{
    let html = "<p>Olá, mundo!</p><a>ok</a>";
    let result = parse_p_tag(html);

    match result {
        Ok((html, m)) => {
            let tag = extract_html_values(&html).unwrap().0;
            //
            println!("text: {:#?}, tag: {}, remain: {:?}", extract_text(&html), tag, m)
        },
        Err(error) => println!("Erro: {}", error),
    }
}