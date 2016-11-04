ls src | sed 's/.elm//' | xargs -I _ echo elm make src/_.elm --output docs/_.html
