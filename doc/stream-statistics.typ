// Estimating the stream of a river from rowing GPS data
#set page(paper: "a4", margin: 2cm, numbering: "1")
#set text(size: 10.5pt)
#set par(justify: true)
#set heading(numbering: "1.")
#show heading: set block(above: 1.3em, below: 0.7em)
#show figure.where(kind: table): set figure.caption(position: top)
#show figure: set block(breakable: false)

// ---- helpers ---------------------------------------------------------
#let booktab(header: (), rows: (), columns: none, align: center, caption: none, size: auto) = {
  let cols = if columns == none { header.len() } else { columns }
  let cells = ()
  for r in rows { for c in r { cells.push([#c]) } }
  figure(
    {
      if size != auto { set text(size: size) }
      table(
        columns: cols, align: align, stroke: none, inset: (x: 5pt, y: 3.5pt),
        table.hline(stroke: 0.9pt),
        table.header(..header.map(h => strong(h))),
        table.hline(stroke: 0.5pt),
        ..cells,
        table.hline(stroke: 0.9pt),
      )
    },
    caption: caption,
  )
}

// number with exactly d decimals and a real minus sign
#let fmt(x, d: 3) = {
  let neg = x < 0
  let i = int(calc.round(calc.abs(x) * calc.pow(10, d)))
  let t = str(i)
  while t.len() <= d { t = "0" + t }
  let whole = t.slice(0, t.len() - d)
  let frac = t.slice(t.len() - d)
  (if neg and i != 0 { "−" } else { "" }) + whole + (if d > 0 { "." + frac } else { "" })
}

#let down-col = rgb("#d95f02")
#let up-col = rgb("#1b9e77")
#let p = 1.0 / 3.0

// sums of the regression of speed on effort e = s^p and direction d
#let zero-sums = (n: 0, see: 0.0, sed: 0.0, sdd: 0.0, sey: 0.0, sdy: 0.0, syy: 0.0)
#let add-sums(a, b) = (
  n: a.n + b.n, see: a.see + b.see, sed: a.sed + b.sed, sdd: a.sdd + b.sdd,
  sey: a.sey + b.sey, sdy: a.sdy + b.sdy, syy: a.syy + b.syy,
)
// pts: array of (d, s, v)
#let sums-of(pts) = {
  let t = zero-sums
  for (d, s, v) in pts {
    let e = calc.pow(s, p)
    t = add-sums(t, (n: 1, see: e * e, sed: e * d, sdd: 1.0, sey: e * v, sdy: d * v, syy: v * v))
  }
  t
}
// least squares; none if degenerate
#let solve(s) = {
  let det = s.see * s.sdd - s.sed * s.sed
  if s.n < 3 or det <= 1e-12 * s.see * s.sdd { return none }
  let k = (s.sdd * s.sey - s.sed * s.sdy) / det
  let u = (s.see * s.sdy - s.sed * s.sey) / det
  let rss = calc.max(0.0, s.syy - k * s.sey - u * s.sdy)
  let var = rss / (s.n - 2)
  (k: k, u: u, rss: rss, det: det, sigma: calc.sqrt(var),
   se-u: calc.sqrt(var * s.see / det), se-k: calc.sqrt(var * s.sdd / det))
}
#let percentile(q, xs) = {
  let pos = q * (xs.len() - 1)
  let lo = calc.floor(pos)
  let hi = calc.min(xs.len() - 1, lo + 1)
  xs.at(lo) + (pos - lo) * (xs.at(hi) - xs.at(lo))
}
#let lcg(x) = calc.rem(1103515245 * x + 12345, 2147483648)

// ---- toy data (synthetic, for the worked example) ---------------------
// (block, d, stroke rate s, speed v)
#let toy = (
  (1, 1, 18, 3.49),
  (1, 1, 18, 3.46),
  (2, -1, 18, 3.21),
  (2, -1, 19, 3.24),
  (3, 1, 19, 3.44),
  (3, 1, 20, 3.53),
  (4, -1, 19, 3.32),
  (4, -1, 20, 3.41),
  (5, 1, 20, 3.58),
  (5, 1, 20, 3.59),
  (6, -1, 24, 3.6),
  (6, -1, 25, 3.68),
  (7, 1, 18, 3.4),
  (7, 1, 19, 3.43),
  (8, -1, 19, 3.25),
  (8, -1, 19, 3.28),
  (9, 1, 20, 3.6),
  (9, 1, 21, 3.62),
  (10, -1, 20, 3.31),
  (10, -1, 21, 3.39),
  (11, 1, 19, 3.51),
  (11, 1, 19, 3.5),
  (12, -1, 18, 3.25),
  (12, -1, 18, 3.24),
)
#let toy-pts = toy.map(r => (r.at(1), r.at(2), r.at(3)))
#let toy-sums = sums-of(toy-pts)
#let toy-fit = solve(toy-sums)
#let toy-blocks = range(1, 13).map(b => sums-of(toy.filter(r => r.at(0) == b).map(r => (r.at(1), r.at(2), r.at(3)))))

// 100 bootstrap replicates of the 12 blocks, drawn with a small generator
#let toy-reps = {
  let x = 20261015
  let out = ()
  for r in range(100) {
    let idx = ()
    for j in range(12) {
      x = lcg(x)
      idx.push(calc.floor(12 * x / 2147483648))
    }
    let tot = zero-sums
    for i in idx { tot = add-sums(tot, toy-blocks.at(i)) }
    let f = solve(tot)
    out.push((idx: idx.map(i => i + 1), u: if f == none { none } else { f.u }))
  }
  out
}
#let toy-us = toy-reps.filter(r => r.u != none).map(r => r.u).sorted()
#let toy-failed = toy-reps.len() - toy-us.len()
#let toy-mean = toy-us.sum() / toy-us.len()
#let toy-sd = calc.sqrt(toy-us.map(x => calc.pow(x - toy-mean, 2)).sum() / (toy-us.len() - 1))

// ---- real data (Norwich sessions, see section 7) ----------------------
#let s1 = (n: 647, down: 254, up: 393, sums: (n: 647, see: 4872.135754736213, sed: -443.9725188481345, sdd: 647.0, sey: 6184.731717262045, sdy: -502.6299999999997, syy: 7870.732699999984), rho: 0.5743837397918341, neff: 174.90889507731666, nb: 42, best: 0.36, boot: (0.07255676214193557, 0.13159031587785544, 0.015567289075486239), iid: (0.08871291120257906, 0.11203780785167872, 0.005984607484941936))
#let s1-fit = solve(s1.sums)
#let s2 = (n: 663, down: 247, up: 416, sums: (n: 663, see: 5355.56570271905, sed: -581.5753092175455, sdd: 663.0, sey: 6656.195215866033, sdy: -635.8999999999999, syy: 8297.956999999995), rho: 0.6036622323967952, neff: 163.85741000347198, nb: 39, best: 0.35, boot: (0.12189878451368323, 0.16845301208666047, 0.011980779510396555), iid: (0.1353813664932999, 0.1542279285243799, 0.005010373208662639))
#let s2-fit = solve(s2.sums)
#let hist-lo = 0.04
#let hist-step = 0.005
#let hist-counts = (0, 0, 0, 1, 3, 6, 8, 22, 33, 42, 56, 53, 61, 65, 53, 37, 21, 20, 13, 3, 1, 2, 0, 0)


// ---- figures ----------------------------------------------------------
#let toy-plot(w: 11.5cm, h: 6.2cm) = {
  let xlo = 2.55
  let xhi = 2.95
  let ylo = 3.1
  let yhi = 3.9
  let X(e) = (e - xlo) / (xhi - xlo) * w
  let Y(v) = h - (v - ylo) / (yhi - ylo) * h
  box(width: w + 1.2cm, height: h + 1.0cm, inset: (left: 1.0cm), {
    place(left + top, rect(width: w, height: h, stroke: 0.5pt + luma(150)))
    for t in (2.6, 2.7, 2.8, 2.9) {
      place(left + top, dx: X(t) - 0.5cm, dy: h + 3pt, box(width: 1cm, align(center, text(size: 8pt)[#fmt(t, d: 1)])))
    }
    for t in (3.2, 3.4, 3.6, 3.8) {
      place(left + top, dx: -0.95cm, dy: Y(t) - 0.5em, box(width: 0.8cm, align(right, text(size: 8pt)[#fmt(t, d: 1)])))
    }
    for sgn in (1, -1) {
      let col = if sgn == 1 { down-col } else { up-col }
      place(left + top, line(start: (X(xlo), Y(toy-fit.k * xlo + sgn * toy-fit.u)),
        end: (X(xhi), Y(toy-fit.k * xhi + sgn * toy-fit.u)), stroke: 1.2pt + col))
    }
    for (b, d, s, v) in toy {
      let col = if d == 1 { down-col } else { up-col }
      place(left + top, dx: X(calc.pow(s, p)) - 2.5pt, dy: Y(v) - 2.5pt, circle(radius: 2.5pt, fill: col, stroke: none))
    }
    place(left + top, dx: 0pt, dy: h + 0.55cm, box(width: w, align(center, text(size: 9pt)[effort $e = s^(1 slash 3)$ (stroke rate $s$ in strokes per minute)])))
    place(left + top, dx: X(2.80), dy: Y(3.28), text(size: 8.5pt)[#box(circle(radius: 2.5pt, fill: down-col, stroke: none)) downstream \ #box(circle(radius: 2.5pt, fill: up-col, stroke: none)) upstream])
  })
}

#let histogram(counts, lo, step, marks, w: 13cm, h: 4cm) = {
  let n = counts.len()
  let mx = calc.max(..counts)
  let hi = lo + step * n
  let X(v) = (v - lo) / (hi - lo) * w
  box(width: w, height: h + 1.5cm, {
    for (i, c) in counts.enumerate() {
      let bh = c / mx * h
      place(left + top, dx: X(lo + i * step), dy: h - bh, rect(width: w / n - 0.6pt, height: bh, fill: luma(170), stroke: none))
    }
    place(left + top, dy: h, line(length: w, stroke: 0.5pt))
    for i in range(0, n + 1, step: 4) {
      let t = lo + i * step
      place(left + top, dx: X(t) - 0.6cm, dy: h + 3pt, box(width: 1.2cm, align(center, text(size: 8pt)[#fmt(t, d: 3)])))
    }
    for m in marks {
      place(left + top, dx: X(m.at(0)), dy: 0pt, line(start: (0pt, 0pt), end: (0pt, h), stroke: (paint: m.at(1), thickness: 1.2pt, dash: m.at(2))))
    }
    place(left + top, dy: h + 0.65cm, box(width: w, align(center, text(size: 9pt)[stream $u$ in m/s, one value per bootstrap replicate])))
  })
}

// ---- document -----------------------------------------------------------
#align(center)[
  #text(size: 17pt, weight: "bold")[Estimating the stream of a river from rowing GPS data]
  #v(2pt)
  #text(size: 11pt)[Statistics, the block bootstrap, and worked examples]
]

= Overview

A crew rows out and back on a river. Going downstream the stream adds to the boat's speed over the ground, going upstream it takes speed away. If the crew rowed with exactly the same effort in both directions, the stream would simply be half the difference between the two speeds. In practice the effort varies: the stroke rate changes during a session and a faster rate gives a faster boat. The method therefore compares speeds *at equal effort*, with the stroke rate $s$ as the measure of effort, and asks which single stream speed $u$ best explains all observed speeds.

The ingredients are

+ a model of speed as a function of stroke rate and direction (@sec-model),
+ labelling every GPS point as upstream or downstream and discarding unusable points (@sec-data),
+ a least-squares fit that yields the stream $hat(u)$ together with a formula-based standard error (@sec-ls),
+ a *block bootstrap* that gives an honest confidence interval, because neighbouring GPS samples are strongly correlated (@sec-boot),
+ diagnostics and the conditions under which no stream can be estimated (@sec-diag).

@sec-ex1 and @sec-ex2 contain worked examples: a small synthetic one that can be followed by hand, and the two real sessions on the River Cam from Norwich. The numbers of the synthetic example are computed by this document from the data shown. For the real sessions the least-squares results are computed here from the six sums; the autocorrelation and the bootstrap summaries come from the software.

= The model <sec-model>

Let $v_i$ be the ground speed in m/s of GPS point $i$ and $s_i$ its stroke rate in strokes per minute. Let $d_i = +1$ if the boat moves downstream at that moment and $d_i = -1$ if it moves upstream. The model is

$ v_i = k s_i^p + d_i u + epsilon_i , quad p = 1/3 . $

- $k s_i^p$ is the speed through the water. $k$ is a constant of the crew and boat on that day.
- $d_i u$ is the stream. $u$ is positive if the water flows downstream. It enters with the same size in both directions and with opposite signs.
- $epsilon_i$ is everything else: GPS noise, the stroke-to-stroke speed oscillation, wind, differences in power per stroke.

*Why $p = 1 slash 3$.* If each stroke takes about the same work, the power the crew delivers is proportional to the stroke rate $s$. The power needed to move a boat against water drag grows with the cube of the speed through the water. Equating the two gives $v prop s^(1 slash 3)$. The exponent is an assumption, checked in @sec-diag by trying other values.

Write $e_i = s_i^p$ for the *effort* of point $i$. The model is then $v_i = k e_i + d_i u + epsilon_i$, linear in the two unknowns $k$ and $u$.

= From GPS trace to regression data <sec-data>

*Direction.* The positions are converted to local metres. The principal axis of the track is taken as the river axis and oriented towards a chosen compass bearing, which defines "downstream" (north for the Cam towards Ely). Each point gets its position along that axis, and its direction is the sign of the slope of the along-axis position against time, fitted by least squares over a window of $plus.minus 15$ s around the point. A single GPS jitter therefore cannot flip the direction.

*Selection.* A point is used if its speed is between 3 and 6 m/s and its stroke rate is positive and at most 40 strokes per minute. This removes turns, starting and paddling, GPS glitches and implausible stroke-rate readings. A direction needs at least 20 usable points.

= Least squares <sec-ls>

Collect the $n$ used points. The least-squares estimates minimise the residual sum of squares $"RSS" = sum_i (v_i - k e_i - d_i u)^2$. Setting the derivatives to zero gives the normal equations

$ mat(S_(e e), S_(e d); S_(e d), S_(d d)) vec(k, u) = vec(S_(e v), S_(d v)) $

with the sums $S_(e e) = sum e_i^2$, $S_(e d) = sum e_i d_i$, $S_(d d) = sum d_i^2 = n$, $S_(e v) = sum e_i v_i$, $S_(d v) = sum d_i v_i$ and $S_(v v) = sum v_i^2$. With $D = S_(e e) S_(d d) - S_(e d)^2$ the solution is

$ hat(k) = (S_(d d) S_(e v) - S_(e d) S_(d v)) / D , quad hat(u) = (S_(e e) S_(d v) - S_(e d) S_(e v)) / D . $

Everything the fit needs is contained in these six sums, a fact that makes the bootstrap cheap (@sec-boot-impl). The residual variance and the standard errors follow from the standard formulas:

$ "RSS" = S_(v v) - hat(k) S_(e v) - hat(u) S_(d v) , quad hat(sigma)^2 = "RSS" / (n - 2) , quad "SE"(hat(u)) = hat(sigma) sqrt(S_(e e) / D) , quad "SE"(hat(k)) = hat(sigma) sqrt(S_(d d) / D) . $

The usual approximate 95% interval is $hat(u) plus.minus 1.96 "SE"(hat(u))$. It is only valid if the errors $epsilon_i$ are independent, which they are not (@sec-boot-why).

*What identifies the stream.* In the plane of effort $e$ against speed $v$ the downstream points lie on the line $v = k e + u$ and the upstream points on $v = k e - u$ (@fig-toy). They are parallel lines at vertical distance $2u$. The fit finds the common slope $k$ and the offset $u$, and the offset is only meaningful because the two lines can be compared at the *same* effort. That is why the stroke rates used in the two directions have to overlap.

*Why one direction is not enough.* If all points are downstream, $d_i = +1$ for all $i$ and $u$ becomes an ordinary intercept: $v = k e + u$. The fit can still be computed, but the "stream" is now whatever offset makes the through-origin law $k s^p$ fit the data, and it cannot be told apart from an error in the exponent or from a boat that is simply not exactly following $s^(1 slash 3)$. With a single direction there is nothing to compare, so the method reports *no estimate* instead of a number (@sec-diag).

= Worked example 1: a small synthetic dataset <sec-ex1>

The following 24 points are *synthetic*, made up to be followed by hand. They come in twelve blocks of two consecutive points, alternately downstream and upstream, with a block-wide shift to mimic the slow variation of real data. Block 6 (stroke rates 24 and 25) has no downstream counterpart at the same effort; it mainly pins down $k$.

#booktab(
  header: ("Block", "Direction", [Stroke rates $s$ of the two points], [Speeds $v$ (m/s)]),
  rows: range(1, 13).map(b => {
    let r = toy.filter(x => x.at(0) == b)
    (str(b), if r.at(0).at(1) == 1 [down] else [up], r.map(x => str(x.at(2))).join(", "), r.map(x => fmt(x.at(3), d: 2)).join(", "))
  }),
  align: (center, left, center, center),
  caption: [Synthetic example data (the effort is $e = s^(1 slash 3)$).],
  size: 9.5pt,
)

The six sums are $n = S_(d d) = #toy-sums.n$, $S_(e e) = #fmt(toy-sums.see, d: 4)$, $S_(e d) = #fmt(toy-sums.sed, d: 4)$, $S_(e v) = #fmt(toy-sums.sey, d: 4)$, $S_(d v) = #fmt(toy-sums.sdy, d: 2)$ and $S_(v v) = #fmt(toy-sums.syy, d: 4)$. Then $D = #fmt(toy-sums.see, d: 4) dot #toy-sums.n - (#fmt(toy-sums.sed, d: 4))^2 = #fmt(toy-fit.det, d: 3)$ and

$ hat(k) = #fmt(toy-fit.k, d: 4) , quad hat(u) = #fmt(toy-fit.u, d: 4) "m/s" . $

The residual sum of squares is $"RSS" = #fmt(toy-fit.rss, d: 5)$, so $hat(sigma) = #fmt(toy-fit.sigma, d: 4)$ m/s and $"SE"(hat(u)) = #fmt(toy-fit.se-u, d: 4)$ m/s. The formula interval is $hat(u) plus.minus 1.96 dot #fmt(toy-fit.se-u, d: 4)$, i.e. #fmt(toy-fit.u - 1.96 * toy-fit.se-u, d: 3) to #fmt(toy-fit.u + 1.96 * toy-fit.se-u, d: 3) m/s. (The data were generated from $k = 1.27$ and $u = 0.10$ plus block shifts and noise, so the fit scatters around these values.)

#figure(
  toy-plot(),
  caption: [The synthetic points and the fitted parallel lines $v = hat(k) e plus.minus hat(u)$. The vertical distance between the lines is $2 hat(u)$.],
) <fig-toy>

= The bootstrap <sec-boot>

== The idea

The formula-based interval answers the question "how much would $hat(u)$ vary if I could repeat the experiment?" under assumptions about the errors. The bootstrap answers the same question by *doing* the repetition on the computer. We do not have a second session on the same day, but the data contain information on how variable the data are. So we create new datasets by drawing from the observed data with replacement, recompute the estimate on every new dataset, and look at how much the estimates vary. If 95% of the replicated estimates lie between $a$ and $b$, then $[a, b]$ is a 95% interval for the stream.

For independent errors one would resample single points. That is not appropriate here.

== Why resampling single points fails <sec-boot-why>

Consecutive GPS samples are a few seconds apart. The boat speed, the crew's effort, the wind and the stream itself change slowly compared with that, so the residuals $epsilon_i$ of neighbouring points are strongly positively correlated. For the first Norwich session the lag-1 autocorrelation of the residuals is $hat(rho)_1 = #fmt(s1.rho, d: 2)$. If points were independent, the 647 points would carry information worth 647 observations. With first-order autocorrelation $rho$ the *effective* sample size is roughly $n (1 - rho) / (1 + rho)$, here about #int(calc.round(s1.neff)) observations. The formula-based standard error and a bootstrap that resamples single points both assume independent points; they are too small by about the square root of the ratio, and the confidence interval is too narrow. The numbers in @tab-compare show this: the two methods that treat points as independent agree with each other and are both about #fmt(s1.boot.at(2) / s1.iid.at(2), d: 1) times narrower than the block bootstrap.

== The block bootstrap <sec-boot-block>

The remedy is to resample *blocks* of consecutive points, as long as the correlation, instead of single points. Within a block the correlation is kept intact; blocks far apart are close to independent. With block length $L$ (default 60 s):

+ Sort the used points by time and cut the session into consecutive blocks of $L$ seconds. A point belongs to block $floor((t - t_0) / L)$, where $t_0$ is the time of the first used point. Session 1 gives #s1.nb blocks.
+ *Resample.* Draw $b$ block numbers with replacement from the $b$ blocks (so some blocks appear twice or more, others not at all) and concatenate those blocks into a new dataset of about the original size.
+ *Re-estimate.* Fit the model on the new dataset and keep the stream $hat(u)^((r))$. If the new dataset contains only one direction, or too little data, the replicate is discarded.
+ *Repeat* steps 2 and 3 for $B = 500$ replicates.
+ *Percentile interval.* Sort the $B' lt.eq B$ valid replicates $u_((1)) lt.eq dots lt.eq u_((B'))$. The 95% interval runs from the 2.5th to the 97.5th percentile. With the 0-based position $q (B' - 1)$ and linear interpolation between neighbours, $q = 0.025$ and $q = 0.975$ give the two ends. For $B' = 500$ the lower end lies at position 12.475, i.e. 47.5% of the way from the 13th to the 14th smallest value.

The standard deviation of the replicates is a bootstrap standard error of the stream. It is the quantity that should be compared with $"SE"(hat(u))$ from the formula.

== Implementation: block sums <sec-boot-impl>

Because the fit depends on the data only through the six sums of @sec-ls, and sums of a concatenation are the sums of the parts, the sums are computed *once per block*. A replicate then costs only $b$ additions of six numbers and one $2 times 2$ solve, so 500 replicates take well under a second even for hours of data. The random draws come from a seeded generator (the seed is a parameter), which makes results reproducible. Different generators or seeds give slightly different percentile endpoints; with 500 replicates a few thousandths of a m/s.

== Worked example: the bootstrap on the synthetic data <sec-ex1b>

The 24 synthetic points form twelve blocks of two points (@sec-ex1). Resampling means drawing twelve block numbers with replacement. In this document they come from a small linear congruential generator with a fixed seed, so the text can be recomputed; #toy-reps.len() replicates are drawn. @tab-toyreps shows the first eight.

#booktab(
  header: ("Replicate", "Blocks drawn", [$hat(u)^((r))$ (m/s)]),
  rows: toy-reps.slice(0, 8).enumerate().map(((i, r)) => (str(i + 1), r.idx.map(str).join(", "), if r.u == none [discarded: one direction only] else [#fmt(r.u, d: 3)])),
  align: (center, left, center),
  caption: [The first replicates of the toy bootstrap.],
  size: 9.5pt,
) <tab-toyreps>

Replicate 1, for instance, contains block #toy-reps.at(0).idx.at(0) first, then block #toy-reps.at(0).idx.at(1), and so on; a block drawn twice counts twice and a block never drawn does not appear. The six sums of the replicate are the sums of its blocks, and the fit is solved as in @sec-ex1. #if toy-failed > 0 [In #toy-failed of the #toy-reps.len() replicates the draw contained blocks from one direction only, so no stream can be computed and the replicate is discarded.] #if toy-failed == 0 [All #toy-reps.len() replicates contained both directions.]

The #toy-us.len() valid replicates have mean #fmt(toy-mean, d: 3) and standard deviation #fmt(toy-sd, d: 3) m/s; the latter is the bootstrap standard error, to be compared with the formula value #fmt(toy-fit.se-u, d: 3). Sorted, the four smallest are #toy-us.slice(0, 4).map(x => fmt(x, d: 3)).join(", ") and the four largest #toy-us.slice(-4).map(x => fmt(x, d: 3)).join(", "). The 2.5th percentile is at position $0.025 dot (#toy-us.len() - 1) = #fmt(0.025 * (toy-us.len() - 1), d: 3)$, so it lies between the sorted values number #(calc.floor(0.025 * (toy-us.len() - 1)) + 1) and #(calc.floor(0.025 * (toy-us.len() - 1)) + 2) and equals #fmt(percentile(0.025, toy-us), d: 3). In the same way the 97.5th percentile is #fmt(percentile(0.975, toy-us), d: 3). The bootstrap interval for the stream is therefore #fmt(percentile(0.025, toy-us), d: 3) to #fmt(percentile(0.975, toy-us), d: 3) m/s, against #fmt(toy-fit.u - 1.96 * toy-fit.se-u, d: 3) to #fmt(toy-fit.u + 1.96 * toy-fit.se-u, d: 3) m/s from the formula.

With twelve blocks and #toy-reps.len() replicates this is an illustration of the mechanics. With so few blocks a draw can easily over- or under-represent one direction, and the estimate from such an unbalanced replicate is poor; this is one reason why the bootstrap standard error exceeds the formula value here. The real analysis uses about 40 blocks and 500 replicates, where unbalanced draws are rare.

== Choices and limits

- *Block length.* It should be at least as long as the correlation in the residuals, but short enough to give many blocks. 60 s is about 15 to 30 samples. If there are fewer than 10 blocks the interval is unreliable and a warning is given.
- *Percentile interval.* The simple percentile interval is easy to explain and robust for an almost symmetric distribution, as here. It does not correct for bias of the estimator.
- *What it cannot fix.* The bootstrap measures random variation within one session. It does not detect a systematic error such as a wrong downstream bearing, or a stream that changes along the river. These show up in residual plots and in the warnings, not in the interval.

= Worked example 2: the Norwich sessions <sec-ex2>

The two sessions were rowed on the Cam on 12 October 2026 (17:32 to 18:44 UTC) and on 13 October 2026 (04:11 to 05:19 UTC). Downstream is north. The filters of @sec-data leave #s1.n points in session 1 (#s1.down downstream, #s1.up upstream) and #s2.n points in session 2 (#s2.down, #s2.up).

*Session 1 by hand from the sums.* The six sums of the used points are $S_(e e) = #fmt(s1.sums.see, d: 2)$, $S_(e d) = #fmt(s1.sums.sed, d: 2)$, $S_(d d) = #s1.sums.sdd$, $S_(e v) = #fmt(s1.sums.sey, d: 2)$, $S_(d v) = #fmt(s1.sums.sdy, d: 2)$ and $S_(v v) = #fmt(s1.sums.syy, d: 2)$. Hence

$ D = #fmt(s1.sums.see, d: 2) dot #s1.sums.sdd - (#fmt(s1.sums.sed, d: 2))^2 = #fmt(s1-fit.det, d: 0) , $

$ hat(k) = #fmt(s1-fit.k, d: 4) , quad hat(u) = #fmt(s1-fit.u, d: 4) "m/s" , quad hat(sigma) = #fmt(s1-fit.sigma, d: 4) "m/s" , quad "SE"(hat(u)) = #fmt(s1-fit.se-u, d: 4) "m/s" . $

The stream is about 0.10 m/s, 0.36 km/h, downstream. The same computation for session 2 is in @tab-sessions.

#booktab(
  header: ("", "Session 1 (12 Oct)", "Session 2 (13 Oct)"),
  rows: (
    ("Points used (down / up)", [#s1.n (#s1.down / #s1.up)], [#s2.n (#s2.down / #s2.up)]),
    ([Crew constant $hat(k)$], fmt(s1-fit.k, d: 3), fmt(s2-fit.k, d: 3)),
    ([Residual SD $hat(sigma)$ (m/s)], fmt(s1-fit.sigma, d: 3), fmt(s2-fit.sigma, d: 3)),
    ([Stream $hat(u)$ (m/s, downstream)], fmt(s1-fit.u, d: 3), fmt(s2-fit.u, d: 3)),
    ([Stream in km/h], fmt(s1-fit.u * 3.6, d: 2), fmt(s2-fit.u * 3.6, d: 2)),
    ([Residual lag-1 autocorrelation $hat(rho)_1$], fmt(s1.rho, d: 2), fmt(s2.rho, d: 2)),
    ("Blocks of 60 s", str(s1.nb), str(s2.nb)),
  ),
  align: (left, center, center),
  caption: [Least-squares results for the two Norwich sessions.],
  size: 9.5pt,
) <tab-sessions>

*Confidence intervals.* @tab-compare compares three intervals for each session: the formula interval, a bootstrap that resamples single points, and the block bootstrap (500 replicates, seed 1).

#booktab(
  header: ("", "Method", [SD of $hat(u)$ (m/s)], "95% interval (m/s)"),
  rows: (
    ([Session 1], [Formula ($plus.minus 1.96 "SE"$)], fmt(s1-fit.se-u, d: 4), [#fmt(s1-fit.u - 1.96 * s1-fit.se-u, d: 3) to #fmt(s1-fit.u + 1.96 * s1-fit.se-u, d: 3)]),
    ([], "Bootstrap of single points", fmt(s1.iid.at(2), d: 4), [#fmt(s1.iid.at(0), d: 3) to #fmt(s1.iid.at(1), d: 3)]),
    ([], [*Block bootstrap*], fmt(s1.boot.at(2), d: 4), [*#fmt(s1.boot.at(0), d: 3) to #fmt(s1.boot.at(1), d: 3)*]),
    ([Session 2], [Formula ($plus.minus 1.96 "SE"$)], fmt(s2-fit.se-u, d: 4), [#fmt(s2-fit.u - 1.96 * s2-fit.se-u, d: 3) to #fmt(s2-fit.u + 1.96 * s2-fit.se-u, d: 3)]),
    ([], "Bootstrap of single points", fmt(s2.iid.at(2), d: 4), [#fmt(s2.iid.at(0), d: 3) to #fmt(s2.iid.at(1), d: 3)]),
    ([], [*Block bootstrap*], fmt(s2.boot.at(2), d: 4), [*#fmt(s2.boot.at(0), d: 3) to #fmt(s2.boot.at(1), d: 3)*]),
  ),
  align: (left, left, center, center),
  caption: [Three ways to get an interval for the stream.],
  size: 9.5pt,
) <tab-compare>

The first two methods agree, as expected, since the single-point bootstrap just reproduces what the formula assumes. The block bootstrap standard deviation is about #fmt(s1.boot.at(2) / s1.iid.at(2), d: 1) times larger in session 1 and #fmt(s2.boot.at(2) / s2.iid.at(2), d: 1) times larger in session 2. The simple correction $sqrt((1 + rho) / (1 - rho))$ with the lag-1 autocorrelation would predict #fmt(calc.sqrt((1 + s1.rho) / (1 - s1.rho)), d: 1); the observed factor is larger, most likely because the correlation decays more slowly than a first-order model suggests and because the stream is identified by comparing long stretches in the two directions. The conclusion is the same in both cases: the honest uncertainty is about two to three times what the formula claims.

#figure(
  histogram(hist-counts, hist-lo, hist-step, (
    (s1-fit.u, black, "solid"),
    (s1.boot.at(0), rgb("#1f5fbf"), "dashed"),
    (s1.boot.at(1), rgb("#1f5fbf"), "dashed"),
    (s1-fit.u - 1.96 * s1-fit.se-u, rgb("#d95f02"), "dotted"),
    (s1-fit.u + 1.96 * s1-fit.se-u, rgb("#d95f02"), "dotted"),
  )),
  caption: [Distribution of the 500 bootstrap replicates of the stream in session 1. Black: least-squares estimate. Blue dashed: block-bootstrap 95% interval. Orange dotted: formula interval, which is much too narrow.],
) <fig-hist>

The two sessions differ: #fmt(s1-fit.u, d: 3) against #fmt(s2-fit.u, d: 3) m/s. The block-bootstrap intervals overlap slightly, and the difference of #fmt(s2-fit.u - s1-fit.u, d: 3) m/s is, judged against the block bootstrap standard deviations (#fmt(s1.boot.at(2), d: 3) and #fmt(s2.boot.at(2), d: 3)), roughly #fmt((s2-fit.u - s1-fit.u) / calc.sqrt(calc.pow(s1.boot.at(2), 2) + calc.pow(s2.boot.at(2), 2)), d: 1) standard deviations. This is why the stream is estimated *per session*: different days can have different flow, and pooling would hide that.

The web application and the command-line tool report the same least-squares values. Their bootstrap intervals differ slightly in the last digit because they use different random number generators: for session 1 the web application gives #fmt(s1.boot.at(0), d: 3) to #fmt(s1.boot.at(1), d: 3) m/s and the OCaml tool 0.068 to 0.128 m/s.

= Diagnostics and when no stream can be estimated <sec-diag>

- *No estimate.* If all used points are in one direction, the result is "no estimate" with the reason `one_direction_only` (@sec-ls explains why). Likewise `no_usable_points` (nothing passes the filters, for instance a file without stroke rate) and `too_few_points` (fewer than 20 points in a direction).
- *Exponent check.* The same regression is repeated for exponents $p$ from 0.05 to 1.5 in steps of 0.01 and the exponent with the smallest residual sum of squares is reported with the stream it implies. For session 1 the best exponent is #fmt(s1.best, d: 2) and for session 2 #fmt(s2.best, d: 2), close to the assumed 1/3, and the stream barely moves.
- *Shared stroke rate range.* The number of points of each direction inside the range of stroke rates that both directions have in common (5th to 95th percentile) shows how well the comparison at equal effort is supported. A warning is given if the ranges do not overlap or if there are fewer than 20 points of one direction in the common range.
- *Negative stream.* If the estimate is significantly negative, the water seems to flow towards "upstream". This almost always means the downstream bearing was entered the wrong way round, and a warning says so.
- *Few blocks.* Fewer than 10 bootstrap blocks give a warning.

= Using the stream: correcting splits <sec-splits>

Rowers think in splits, the time per 500 m, $T = 500 / v$. A speed observed in the boat is a ground speed. Rowing upstream the boat's speed through the water is $v + u$, rowing downstream it is $v - u$. A table of observed splits can therefore be converted into the split the crew would achieve in still water:

$ T_"true, up" = 500 / (500 / T_"obs" + hat(u)) , quad T_"true, down" = 500 / (500 / T_"obs" - hat(u)) . $

#let split-str(t) = {
  let tenths = int(calc.round(t * 10))
  let m = calc.floor(tenths / 600)
  let s = (tenths - m * 600) / 10
  str(m) + ":" + (if s < 10 { "0" } else { "" }) + fmt(s, d: 1)
}
For example, with $hat(u) = #fmt(s1-fit.u, d: 3)$ m/s from session 1, an observed split of 2:00 corresponds to a true split of #split-str(500 / (500 / 120 + s1-fit.u)) when rowing upstream and #split-str(500 / (500 / 120 - s1-fit.u)) when rowing downstream: the same observed pace is harder to achieve against the stream and easier with it, so the upstream true split is faster than observed and the downstream one slower.

= Assumptions and limitations

- The stream is one constant value for the whole session and the same size in both directions. A stream that varies along the river, or a tide, is averaged.
- The model has no term for wind. A steady wind component along the river acts like a stream; a wind that differs between the outward and the return leg is absorbed into the stream or the residual.
- $k$ is a constant for the session; a crew that rows its upstream pieces at a different quality than its downstream pieces would bias the stream. Overlapping stroke rates reduce this risk but cannot remove it.
- The river axis is a straight line through the track. This is adequate for an out-and-back row on a river, not for loops or basins.
- The exponent $p = 1 slash 3$ is an assumption; the exponent check of @sec-diag shows how sensitive the result is.
- The confidence interval describes random variation within one session. It does not cover systematic errors.
