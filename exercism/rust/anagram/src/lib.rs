fn same_letters(source: &str, input: &str) -> bool {
    let mut input_letters = input.chars().collect::<Vec<_>>();
    let mut source_letters = source.chars().collect::<Vec<_>>();

    input_letters.sort();
    source_letters.sort();

    input_letters == source_letters
}

pub fn anagrams_for<'a>(source: &str, inputs: &[&'a str]) -> Vec<&'a str> {
    let src = source.to_lowercase();

    let references = inputs.iter()
        .filter( | &input |
                    &input.to_lowercase() != &src
                    && same_letters(&input.to_lowercase(), &src)
        );

    let mut anagrams = vec![];

    for reference in references {
        anagrams.push(*reference);
    }

    anagrams
}
