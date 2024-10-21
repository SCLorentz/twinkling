#[derive(Clone, Debug)]
struct Chars<'a>
{
    iter: std::str::Chars<'a>,
}

impl<'a> Chars<'a> {
    fn new(s: &'a str) -> Self {
        Chars { iter: s.chars() }
    }

    fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    fn expect(&mut self, expected: char) -> Result<&Self, String> {
        if self.next() == Some(expected) {
            Ok(self)
        } else {
            Err(format!("missing args: `{}`", expected).to_string())
        }
    }
}

//Chars::new(input)