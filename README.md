# Draw me

Sort of like pictoline made in Elm. I got the name from the song [Draw me - Sonata Arctica][name-origin].

## Requirements
- [asdf][asdf]
- [asdf-elm][asdf-elm]
- [asdf-node][asdf-node]

## Set up

Install requirements:
```
asdf install
```

Install dependencies:
```
npm install
```

## Build

```
npm run build
```

*NOTE:* This will also download the Elm dependencies.

## Watch

Execute elm reactor:
```
npm run watch
```

After that, open [http://localhost:8000][http://localhost:8000] in a browser. Go to `dist > index.html`.

[asdf-elm]: https://github.com/asdf-community/asdf-elm
[asdf]: https://github.com/asdf-vm/asdf
[name-origin]: https://www.youtube.com/watch?v=NozBQG_98JU
[asdf-node]: https://github.com/asdf-vm/asdf-nodejs
