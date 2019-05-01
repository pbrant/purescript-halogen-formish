# purescript-halogen-formish

A proof-of-concept Halogen implementation of the approach discussed here [Using PureScript to create a domain-specific language for building forms with validation](https://medium.com/fuzzy-sharp/building-a-type-safe-embedded-dsl-for-form-components-with-validation-e7ffaaf537e4).

It's been somewhat expanded/modified to account for current form behavior, but is otherwise unstyled and needs rounding out if this looks like a promising way to go.

To run:

```bash
export PATH=$(pwd)/node_modules/.bin:$PATH
yarn install
pulp server
```

The article goes into much greater (and better) detail, but the basic idea is that we'd define client-side forms in a way similar to how server-side processing is done. This would give us considerably more flexibility in terms of returning more precise types while still allowing a similar approach to the current forms framework if that's convenient.

This has a number of advantages, but also some potential pitfalls that we should discuss.
