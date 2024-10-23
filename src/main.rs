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
        HtmlNode::Element { tag, attributes, children } =>
        {
            return Ok((tag, attributes, children))
        }
        _ => return Err("not a node element!".to_string())
    }
}

fn print_result(result: Result<(HtmlNode, &str), String>) -> Result<(), String> {
    match result
    {
        Ok((html, m)) =>
        {
            let tag = extract_html_values(&html)?.0;
            //
            println!("text: {}, tag: {}, remain: {:?}", extract_text(&html), tag, m)
        },
        Err(error) => println!("Erro: {}", error),
    }
    Ok(())
}

fn main() -> Result<(), String>
{
    // o ultimo caractere antes do </x> está sendo cortado
    let html = "<p>Olá, mundo!</p><a>hello world</a>";
    //
    let result = parse_html(html);

    //let _ = print_result(result);
    println!("{:#?}", result);

    Ok(())
}