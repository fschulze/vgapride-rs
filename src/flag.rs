pub const WIDTH: i16 = 640;
pub const HEIGHT: i16 = 480;

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

#[derive(Clone, Debug)]
pub enum PolygonType {
    Filled,
}

#[derive(Clone, Copy, Debug)]
pub struct Point {
    pub x: i16,
    pub y: i16,
}

//sdl-bgi
#[derive(Clone, Debug)]
pub enum Command {
    Rectangle(Point, Point, Color),
    Polygon(PolygonType, Point, Vec<Point>, Color),
}

#[derive(Debug)]
pub struct Commands(Vec<Command>);

impl Commands {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn extend(&mut self, other: &Commands) {
        self.0.extend_from_slice(&other.0)
    }

    pub fn push(&mut self, command: Command) {
        self.0.push(command)
    }
}

impl IntoIterator for Commands {
    type Item = Command;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub struct Flag {
    pub names: Vec<String>,
    pub description: String,
    pub credits: Option<String>,
    pub textsize: i16,
    pub textcolor: Color,
    pub commands: Commands,
}
