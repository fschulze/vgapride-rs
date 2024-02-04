use crate::flag::{Command, Commands, PolygonType};

pub fn render(writer: &mut impl std::io::Write, commands: Commands) -> std::io::Result<()> {
    write!(
        writer,
        "<svg version=\"1.1\" width=\"640\" height=\"480\" xmlns=\"http://www.w3.org/2000/svg\">\n"
    )?;
    for command in commands {
        match command {
            Command::Rectangle(tl, br, color) => {
                write!(
                    writer,
                    "<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"rgb({} {} {})\" />\n",
                    tl.x,
                    tl.y,
                    br.x - tl.x,
                    br.y - tl.y,
                    color.r,
                    color.g,
                    color.b,
                )?;
            }
            Command::Polygon(kind, start, points, color) => {
                write!(writer, "<polygon points=\"")?;
                for (index, point) in points.into_iter().enumerate() {
                    if index > 0 {
                        write!(writer, " ")?;
                    }
                    write!(writer, "{},{}", point.x - start.x, point.y - start.y)?;
                }
                match kind {
                    PolygonType::Filled => {
                        write!(
                            writer,
                            "\" fill=\"rgb({} {} {})\" />\n",
                            color.r, color.g, color.b
                        )?;
                    }
                }
            }
        }
    }
    write!(writer, "</svg>")?;
    writer.flush()?;
    Ok(())
}
