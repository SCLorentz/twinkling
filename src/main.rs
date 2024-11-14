mod parser;
use parser::*;

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
                result += &extract_text(child);
            }
            result
        }
    }
}

fn extract_html_values(node: &HtmlNode) -> Result<(&String, &Option<Vec<(String, String)>>, &Vec<HtmlNode>), String>
{
    match node
    {
        HtmlNode::Element { tag, attributes, children } => return Ok((tag, attributes, children)),
        _ => return Err("not a node element!".to_string())
    }
}

#[allow(unused)]
fn print_result(result: HtmlNode) -> Result<(), String>
{
    let tag = extract_html_values(&result)?.0;
    //
    println!("text: {}, tag: {}", extract_text(&result), tag);
    //
    Ok(())
}

fn main() -> Result<(), String>
{
    let html = "<p>Olá, mundo!</p><a>hello world<p>other</p></a>";
    //
    let result = parse_html(html)?;

    //let _ = print_result(result[1].clone());
    println!("{:#?}", result);

    Ok(())
}