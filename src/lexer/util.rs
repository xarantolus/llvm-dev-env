pub fn get_line_and_column(input: &str, pos: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 0;

    for (i, c) in input.chars().enumerate() {
        if i == pos {
            break;
        }

        if c == '\n' {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }

    (line, column)
}
