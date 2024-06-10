use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Keyword {
    Proc,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let keyword = match s {
            "proc" => Self::Proc,
            _ => return Err(()),
        };

        Ok(keyword)
    }
}
