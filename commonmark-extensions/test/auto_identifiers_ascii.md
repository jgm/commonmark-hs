## Auto identifiers ASCII (extension)

The `auto_identifiers_ascii` extension is like
`auto_identifiers` but limits identifiers to ASCII.
Accented Latin characters are converted into the
closest ASCII equivalent.

```````````````````````````````` example
# Heading with &auml;
.
<h1 id="heading-with-a">Heading with &auml;</h1>
````````````````````````````````
