## Auto identifiers ASCII (extension)

The `auto_identifiers_ascii` extension is like
`auto_identifiers` but limits identifiers to ASCII.
Accented Latin characters are converted into the
closest ASCII equivalent. Other non-ASCII characters
are simply omitted.

```````````````````````````````` example
# Heading with &auml; and &alpha;
.
<h1 id="heading-with-a-and-">Heading with ä and α</h1>
````````````````````````````````

```````````````````````````````` example
# Heading with ❤️
.
<h1 id="heading-with-heart">Heading with ❤️</h1>
````````````````````````````````
