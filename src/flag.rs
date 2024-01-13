#[derive(Clone, Copy, Debug)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

#[derive(Debug)]
pub struct Flag {
    pub names: Vec<String>,
    pub description: String,
    pub credits: Option<String>,
    pub textsize: i16,
    pub textcolor: Color,
}
