
<!-- README.md is generated from README.Rmd. Please edit that file -->

# assessr

<!-- badges: start -->

<!-- badges: end -->

The `assessr` app can be used to display the contributed poster/talk/workshop sessions submitted for the [Bioconductor 2020](https://bioc2020.bioconductor.org/) conference.

# Prerequisites

## Input file(s)

Excel files with columns: "Id", "Title", "Description", "Session format", "Track", "Keywords (1-3)", "Link", "First time presenting this?", "Speaker Notes", "Reviewer1", "Reviewer2", "Reviewer3" 

Currently, files `BioC2020_workshops.xlsx` and `BioC2020_talks_posters.xlsx` are used. They contain data about workshops and talks/posters, respectively.

## Reviewer's working with the app

- Select you name in the "Reviewer name" drop-down menu to see all posters assignned to you.
- Click on each poster - the rightmost panel will display the full information about the submission, and the link to a Google form where one can enter evaluations.

## Output

- Google forms for evaluation should be created _a priori_. Examples of forms are: [eRum 2020 - Workshop Sessions Evaluation](https://docs.google.com/forms/d/e/1FAIpQLScTMLTJ1ccfBmjdEPhZfk5CyQwqSAW5AUJyDkFxc7Q9ZW6VPQ/viewform?entry.840333480=), [eRum 2020 - Contributed Sessions Evaluation](https://docs.google.com/forms/d/e/1FAIpQLSezGbJ1JmgOwDI5BLl28gXp3YQfoFXq8GoMon3k9PZcePCF_w/viewform?entry.840333480=)
- `assessr` will pre-populate the forms with the "Id|Title" string.
- Complete your evaluation by submitting the form.
- Return to `assessr` and complete other evaluations assigned to you.


# Footnotes

**Work in progress! The majority of things are hard-coded and need to be manually adjusted.**
