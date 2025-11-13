# Contributing to CDuce

Thank you for your interest in contributing to CDuce!

The normal way of contributing is the following:

- Create an issue explaining your bug/the change you want to see/something else
- Create a merge request with the `Draft` tag from a new branch that is based on the `dev` branch
- Wait for the reviewers to approve or reject your merge request
- In case of approval, rebase, if needed, your branch on `dev`, squash your commits to limit them to the essential ones and remove the `Draft` tag
- Your branch will be merged as soon as possible

## Raising an [issue]

Go to the [issue] page, try to be as thorough as possible, we'll reach out to you as soon as we can. If your issue is a bug that is not about the `dev` or `main` branch, please make sure that it can be replicated on one of these two since they are the branches where all the things happen.

## Creating a merge request on the `dev` branch

For this, you'll need to do things in order:

- Fork this repository
- `git checkout -b my_new_branch`
- Make your changes in this branch, commit and push
- (Optional but recommended) `git checkout -b my_new_branch-for-ci`, push. Any
  branches whose name ends in `-for-ci` triggers the CI scripts.
- Create a merge request from `my_new_branch` to `dev` (not `main`) with its title starting with: `Draft: `

## Releases

The `dev` branch is the unstable branch on which all new features are tested. 
 - Maintainers and developers can directly push on it even though it's highly not recommended
 - Contributors can't push nor merge branches to `dev` and should go the usual route: issue > merge request > review > merge in the branch

The `main` branch is the (hopefully) stable branch 
 - No one can push in this branch
 - Only maintainers can merge in it
 - Developers and contributors should focus on the `dev` branch and have no permissions on this one

[issue]: https://gitlab.math.univ-paris-diderot.fr/cduce/cduce/-/issues
