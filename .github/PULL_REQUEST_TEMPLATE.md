## What this changes and why

<!-- The "why" matters more than the "what" here — see CONTRIBUTING.md. -->

## Checklist

- [ ] `sbt test` passes locally (full suite, not just the touched module)
- [ ] If this changes any fragment/language/artifact shape: golden digests
      were regenerated deliberately (`sbt "examples/runMain cairn.examples.Main digests"`)
      and the diff is explained above, not just committed silently
- [ ] If this touches `content/languages/*.cairn`: `emit-languages` was run
      and the result committed
- [ ] No new runtime dependency was added to `kernel` / `content/core` /
      `container/system-handler`
- [ ] No new `content/user ↛ system-handler` import was introduced
- [ ] New end-to-end behavior has a transcript or extends an existing one
      (not just a unit test), where applicable

## Related issue

<!-- Closes #... , if any -->
