# Sketches

> Experiments, half baked ideas & unfinished projects.

I have a lot of project ideas that go nowhere.

Rather than lose the code on a harddrive somewhere or flood my GitHub
account with barely working code, I start my new projects here.

When they start to come into shape (and, well, work), I'll split them out
into their own repos using the subtree commands below.

Unless otherwised noted, all code in this repository falls under the 
Affero GNU General Public License ([AGPL-3.0-or-later]).

[AGPL-3.0-or-later]: https://spdx.org/licenses/AGPL-3.0-or-later.html

## Importing existing projects.

```console
$ git subtree add -P [prefix] repo.git master
```

## Splitting sketches out into standalone projects.

```console
$ git subtree push -P [prefix] repo.git master
```
