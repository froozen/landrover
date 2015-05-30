landrover
=========

A tool for converting [markdown][markdown] files into [reveal.js][reveal_js] presentations.
It is inspired by [hovercraft][hovercraft], a tool that converts reStructuredText files into
[impress.js][impress_js] presentations.

### Installation
To install it, clone it and let cabal do the rest:

```
git clone https://github.com/froozen/landrover.git
cd landrover
cabal install
```

### Usage
To use it, you'll first need a reveal.js presentation. The easiest way to get one is cloning
the original repo.

```
git clone https://github.com/hakimel/reveal.js/
```

Next, you'll need a template. The easiest way of getting one is using the one I made.
```
cd reveal.js
wget https://gist.githubusercontent.com/froozen/1ce43a536abbe83534f3/raw/ed990d20d459e9ee53bd3c3f178f0d3d093e4fbe/template.html
```

Now, you can start creating your slides:
```markdown
Example presentation
====================

------

This is a list:
* Eggs
* Ham
* Milk
```

The slides are seperated by horizontal rules.

The last thing you'll have to do is running `landrover`:
```
landrover slides.md presentation
```

The first argument is the path to your markdown slides, the second one the path to the reveal.js
presentation directory.

[markdown]: https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
[reveal_js]: https://github.com/hakimel/reveal.js/
[hovercraft]: https://github.com/regebro/hovercraft
[impress_js]: https://github.com/bartaz/impress.js/
