# Generative AI and the future of survey measurement

A 23-slide Quarto / Reveal.js deck in the same 1920×1080 raw-HTML format and look as `/Users/p.sturgis/Localprojects/cork-talk/quarto/deck.qmd` (cream paper, burgundy accent, Charter serif body, Avenir Next kickers, `deck.css` copied from that project with table and logo styles added). Each slide is a `## {.slide}` heading with an HTML block and a `::: {.notes}` block. The deck is for the European Social Survey and NatCen Survey Methodology Seminar Series, Tuesday 15 September 2026, 12:00–13:00 BST, online. Speaker notes allocate 40 minutes to the talk and 5 minutes to discussion. There is no backup slide; the deck ends with a papers-and-links slide after the Cogbot conclusions. Merged or dropped text slides (project team, static versus dynamic integration, follow-up rules, study 1 design, disagreement patterns, probe types, live-survey composition, repeatability, expert review setup, repeated runs, respondent confidence, severity filtering) survive as speaker notes and captions on the slides that absorbed them.

Open `socbot-cogbot.html` in a browser. Arrow keys advance, F enters full screen, Esc shows the overview, and S opens speaker view. The HTML embeds its assets and can be copied on its own. Speaker notes are included in the HTML.

Edit `socbot-cogbot.qmd`, then render with:

```sh
quarto render socbot-cogbot.qmd
```

On this Mac, Quarto is bundled with RStudio:

```sh
/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto render socbot-cogbot.qmd
```

For rehearsal with speaker view, serve the deck locally with `quarto preview socbot-cogbot.qmd` and then press S.

## Sources

The deck follows the September 2026 versions of the two papers in `papers/`:

- `papers/SMR_anonymousV1 (1).pdf`: SOCbot manuscript (Sturgis, Robinson, Fung and Roberts). Table 1 (agreement by model), Figures 2–6, sections 4.2–4.5 and the discussion.
- `papers/Cogbot_paper_V1 (11).pdf`: "LLMs for survey pretesting" manuscript. Tables 1–5, the human assessment of unmatched findings, and Appendix F (costs).

Original PDF figures and browser PNG versions are in `assets/`. `cog-pipeline-crop.png` is the figure region cut from `cog-pipeline.png` so it can be shown without CSS cropping. The figures are unchanged from the June 2026 source decks and match the figures in the September papers. Text and tables are editable in Quarto.

## Editorial decisions

Title slide names the seminar series and date, with no product names. Framing on the second slide follows the seminar abstract.

SOCbot: the probe-count caption uses the values printed in Figure 4 (23.3%, 49.6%, 20.4%, 5.1%). The reliability caption reports all three pairwise agreements at unit level (58%, 53%, 43%) as in Figure 6. New slides cover the four disagreement patterns (section 4.2) and the compositional explanation for lower live-survey agreement (section 4.4). The accuracy ordering is described as an inference conditional on treating dynamic SOCbot as the better-informed benchmark.

Cogbot: the September paper reverses the results attached to the ER-Open and ER-Restrained labels relative to the June deck (ER-Open now 69% detection with 3.1 unmatched findings per ESS item; ER-Restrained 48% and 0.3). All tables use the September labels. The cross-model comparison uses the four-LLM Table 5, including Claude Opus 5 and provisional false positives per ESS item, in place of the earlier three-model detection figures, whose Llama and Qwen values differ from the current paper. New slides cover repeated runs (Table 3), simulated respondent confidence, severity filtering, and the specialist assessment of 79 unmatched findings. The earlier claim that adjudication judged most false positives genuine is replaced by the paper's split result (66% of CI-List findings on flawed items, 33% for ER-Checklist, near zero on ESS controls).

Model comparisons and cost references describe the study runs at the dates given in the papers, not current capabilities or prices.
