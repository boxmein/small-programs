// --- Day 10: Syntax Scoring ---

// You ask the submarine to determine the best route out of the deep-sea cave, but it only replies:

// Syntax error in navigation subsystem on line: all of them

// All of them?! The damage is worse than you thought. You bring up a copy of the navigation subsystem (your puzzle input).

// The navigation subsystem syntax is made of several lines containing chunks. There are one or more chunks on each line, and chunks contain zero or more other chunks. Adjacent chunks are not separated by any delimiter; if one chunk stops, the next chunk (if any) can immediately start. Every chunk must open and close with one of four legal pairs of matching characters:

//     If a chunk opens with (, it must close with ).
//     If a chunk opens with [, it must close with ].
//     If a chunk opens with {, it must close with }.
//     If a chunk opens with <, it must close with >.

// So, () is a legal chunk that contains no other chunks, as is []. More complex but valid chunks include ([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], and even (((((((((()))))))))).

// Some lines are incomplete, but others are corrupted. Find and discard the corrupted lines first.

// A corrupted line is one where a chunk closes with the wrong character - that is, where the characters it opens and closes with do not form one of the four legal pairs listed above.

// Examples of corrupted chunks include (], {()()()>, (((()))}, and <([]){()}[{}]). Such a chunk can appear anywhere within a line, and its presence causes the whole line to be considered corrupted.

// For example, consider the following navigation subsystem:

// [({(<(())[]>[[{[]{<()<>>
// [(()[<>])]({[<{<<[]>>(
// {([(<{}[<>[]}>{[]{[(<()>
// (((({<>}<{<{<>}{[]{[]{}
// [[<[([]))<([[{}[[()]]]
// [{[{({}]{}}([{[{{{}}([]
// {<[[]]>}<{[{[{[]{()[[[]
// [<(<(<(<{}))><([]([]()
// <{([([[(<>()){}]>(<<{{
// <{([{{}}[<[[[<>{}]]]>[]]

// Some of the lines aren't corrupted, just incomplete; you can ignore these lines for now. The remaining five lines are corrupted:

//     {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
//     [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
//     [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
//     [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
//     <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.

// Stop at the first incorrect closing character on each corrupted line.

// Did you know that syntax checkers actually have contests to see who can get the high score for syntax errors in a file? It's true! To calculate the syntax error score for a line, take the first illegal character on the line and look it up in the following table:

//     ): 3 points.
//     ]: 57 points.
//     }: 1197 points.
//     >: 25137 points.

// In the above example, an illegal ) was found twice (2*3 = 6 points), an illegal ] was found once (57 points), an illegal } was found once (1197 points), and an illegal > was found once (25137 points). So, the total syntax error score for this file is 6+57+1197+25137 = 26397 points!

// Find the first illegal character in each corrupted line of the navigation subsystem. What is the total syntax error score for those errors?

// Your puzzle answer was 415953.
// --- Part Two ---

// Now, discard the corrupted lines. The remaining lines are incomplete.

// Incomplete lines don't have any incorrect characters - instead, they're missing some closing characters at the end of the line. To repair the navigation subsystem, you just need to figure out the sequence of closing characters that complete all open chunks in the line.

// You can only use closing characters (), ], }, or >), and you must add them in the correct order so that only legal pairs are formed and all chunks end up closed.

// In the example above, there are five incomplete lines:

//     [({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})].
//     [(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
//     (((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
//     {<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
//     <{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.

// Did you know that autocomplete tools also have contests? It's true! The score is determined by considering the completion string character-by-character. Start with a total score of 0. Then, for each character, multiply the total score by 5 and then increase the total score by the point value given for the character in the following table:

//     ): 1 point.
//     ]: 2 points.
//     }: 3 points.
//     >: 4 points.

// So, the last completion string above - ])}> - would be scored as follows:

//     Start with a total score of 0.
//     Multiply the total score by 5 to get 0, then add the value of ] (2) to get a new total score of 2.
//     Multiply the total score by 5 to get 10, then add the value of ) (1) to get a new total score of 11.
//     Multiply the total score by 5 to get 55, then add the value of } (3) to get a new total score of 58.
//     Multiply the total score by 5 to get 290, then add the value of > (4) to get a new total score of 294.

// The five lines' completion strings have total scores as follows:

//     }}]])})] - 288957 total points.
//     )}>]}) - 5566 total points.
//     }}>}>)))) - 1480781 total points.
//     ]]}}]}]}> - 995444 total points.
//     ])}> - 294 total points.

// Autocomplete tools are an odd bunch: the winner is found by sorting all of the scores and then taking the middle score. (There will always be an odd number of scores to consider.) In this example, the middle score is 288957 because there are the same number of scores smaller and larger than it.

// Find the completion string for each incomplete line, score the completion strings, and sort the scores. What is the middle score?

use std::collections::VecDeque;
use std::io::{stdin, BufRead};

#[derive(Debug, PartialEq, Eq)]
enum ErrorScanStatus {
    Correct,
    Incomplete { complete_by_adding: Vec<char> },
    IllegalCharacter { character: char, position: usize },
}

fn score(c: &char) -> u32 {
    match *c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

fn corresponding_character(c: &char) -> char {
    match c {
        ')' => '(',
        ']' => '[',
        '}' => '{',
        '>' => '<',
        _ => *c,
    }
}

fn autocompletion_for_character(c: &char) -> char {
    match c {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => *c,
    }
}

fn generate_autocompletion<'a>(parens: impl DoubleEndedIterator<Item = &'a char>) -> Vec<char> {
    parens
        .rev()
        .map(autocompletion_for_character)
        .collect::<Vec<char>>()
}

fn scan_for_errors(inp: impl Iterator<Item = char>) -> ErrorScanStatus {
    let mut parens: VecDeque<char> = VecDeque::new();
    for (i, chr) in inp.enumerate() {
        if "({[<".contains(chr) {
            parens.push_back(chr);
        } else if ")}]>".contains(chr) {
            let corr = corresponding_character(&chr);
            let head = parens.back();

            if head.is_none() {
                println!(
                    "tried to close with {} but stack is empty: {:?}",
                    chr, parens
                );
                return ErrorScanStatus::IllegalCharacter {
                    character: chr,
                    position: i,
                };
            }

            let head = head.unwrap();

            if *head != corr {
                println!(
                    "Expected {}, but found {} ({}) instead on position {}",
                    head, corr, chr, i
                );
                return ErrorScanStatus::IllegalCharacter {
                    character: chr,
                    position: i,
                };
            }

            parens.pop_back();
        }
    }

    if parens.len() > 0 {
        println!("some opening parens still left: {:?}", parens);
        ErrorScanStatus::Incomplete {
            complete_by_adding: generate_autocompletion(parens.iter()),
        }
    } else {
        ErrorScanStatus::Correct
    }
}

fn score_completion_character(c: &char) -> usize {
    match c {
        ')' => 1,
        ']' => 2,
        '}' => 3,
        '>' => 4,
        _ => 0,
    }
}

fn score_completion(completions: &[char]) -> usize {
    let mut score = 0;
    for ch in completions {
        score *= 5;
        score += score_completion_character(&ch);
    }
    score
}

fn main() {
    let errors = stdin()
        .lock()
        .lines()
        .filter(|value| value.is_ok())
        .map(|value| value.expect("stdin failure"))
        .map(|value| scan_for_errors(value.chars()))
        .collect::<Vec<ErrorScanStatus>>();

    println!("{} errors", errors.len());

    // Part 1
    let part_1 = errors.iter().filter(|status| match status {
        ErrorScanStatus::IllegalCharacter { .. } => true,
        _ => false,
    });

    let mut score_1 = 0;
    for error in part_1 {
        match error {
            ErrorScanStatus::IllegalCharacter { character, .. } => {
                score_1 = score(character);
            }
            _ => {}
        }
    }

    println!("part 1: {}", score_1);

    // Part 2
    let mut part_2 = errors
        .iter()
        .filter(|status| match status {
            ErrorScanStatus::Incomplete { .. } => true,
            _ => false,
        })
        .map(|error| match error {
            ErrorScanStatus::Incomplete { complete_by_adding } => {
                score_completion(&complete_by_adding[..])
            }
            _ => panic!("what?"),
        })
        .collect::<Vec<usize>>();

    part_2.sort();

    let score_2 = part_2[part_2.len() / 2usize];

    println!("part 2: {}", score_2);
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! should_return {
        ($s:expr, $r:expr) => {
            assert_eq!(scan_for_errors($s.chars()), $r,);
        };
    }

    #[test]
    fn scan_for_errors_tests() {
        should_return!("()", ErrorScanStatus::Correct);
        should_return!(
            "())",
            ErrorScanStatus::IllegalCharacter {
                character: ')',
                position: 2
            }
        );

        should_return!(
            r#"{([(<{}[<>[]}>{[]{[(<()>"#,
            ErrorScanStatus::IllegalCharacter {
                character: '}',
                position: 12,
            }
        );
        should_return!(
            r#"[[<[([]))<([[{}[[()]]]"#,
            ErrorScanStatus::IllegalCharacter {
                character: ')',
                position: 8,
            }
        );
        should_return!(
            r#"[{[{({}]{}}([{[{{{}}([]"#,
            ErrorScanStatus::IllegalCharacter {
                character: ']',
                position: 7,
            }
        );
        should_return!(
            r#"[<(<(<(<{}))><([]([]()"#,
            ErrorScanStatus::IllegalCharacter {
                character: ')',
                position: 10,
            }
        );
        should_return!(
            r#"<{([([[(<>()){}]>(<<{{"#,
            ErrorScanStatus::IllegalCharacter {
                character: '>',
                position: 16,
            }
        );
    }

    #[test]
    fn generate_autocompletion_tests() {
        assert_eq!(
            generate_autocompletion(vec!['(', '('].iter(),),
            vec![')', ')']
        );
    }

    #[test]
    fn score_part_2_test() {
        assert_eq!(score_completion(&[']', ')', '}', '>']), 294);
        assert_eq!(
            score_completion(&['}', '}', ']', ']', ')', '}', ')', ']']),
            288957
        );
        assert_eq!(score_completion(&[')', '}', '>', ']', '}', ')']), 5566);
        assert_eq!(
            score_completion(&['}', '}', '>', '}', '>', ')', ')', ')', ')']),
            1480781
        );
    }
}
