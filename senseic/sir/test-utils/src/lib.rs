/// Asserts that two strings are equal, showing a detailed diff if they differ.
///
/// # Arguments
/// * `actual` - The actual string value
/// * `expected` - The expected string value
/// * `context_name` - Name to use in the panic message (e.g., "Parse format", "IR display")
/// * `additional_context` - Optional additional context to display (e.g., input that generated the
///   actual value)
pub fn assert_trim_strings_eq_with_diff(actual: &str, expected: &str, context_name: &str) {
    let actual = actual.trim();
    let expected = expected.trim();

    if actual == expected {
        return;
    }

    eprintln!("=== Diff (-expected +actual) ===");

    let expected_lines: Vec<_> = expected.lines().collect();
    let actual_lines: Vec<_> = actual.lines().collect();
    let max_len = expected_lines.len().max(actual_lines.len());

    let mut printed_block = false;
    for idx in 0..max_len {
        let expected_line = expected_lines.get(idx);
        let actual_line = actual_lines.get(idx);

        if expected_line == actual_line {
            continue;
        }

        if printed_block {
            eprintln!();
        }
        printed_block = true;

        let line_no = idx + 1;
        match (expected_line, actual_line) {
            (Some(expected_line), Some(actual_line)) => {
                eprintln!("- line {:>5}: {}", line_no, expected_line);
                eprintln!("+ line {:>5}: {}", line_no, actual_line);
            }
            (Some(expected_line), None) => {
                eprintln!("- line {:>5}: {}", line_no, expected_line);
                eprintln!("+ line {:>5}: <missing>", line_no);
            }
            (None, Some(actual_line)) => {
                eprintln!("- line {:>5}: <missing>", line_no);
                eprintln!("+ line {:>5}: {}", line_no, actual_line);
            }
            (None, None) => unreachable!("handled by max_len iteration"),
        }
    }

    panic!("{} mismatch", context_name);
}
