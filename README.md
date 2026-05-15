# Tesla Workforce Optimization — Linear Programming for Bilingual Support Staffing

> **A linear-programming model that finds the cost-minimizing schedule for Tesla's bilingual customer-support team — saving $100/day (2.2%) while still meeting every demand and language-coverage constraint.**

**R + lpSolve · 2.2% cost reduction · Group 5 case study, MSBA · Santa Clara**

---

## The Problem

Tesla's customer-support function staffs agents in multiple languages across multiple shifts. Workforce schedules built by hand or by spreadsheet typically:

1. **Over-staff to be safe** — easier to add agents than risk a missed-coverage incident
2. **Treat language coverage as a soft constraint** — leading to under-coverage at peak hours
3. **Don't optimize across shifts** — local optima per shift, global misses

Even a small percentage saving compounds at Tesla's scale (a 2.2% saving on a single function, applied across regions, is meaningful annual spend).

> **Why now?** Linear programming has been around for decades, but the bottleneck is *formulation*, not solving. Modern OR + R tooling (`lpSolve`) makes a working LP model achievable in a class week, while the constraint complexity (multi-language, multi-shift, demand-forecast-driven) makes it a real PM-meets-OR problem.

---

## Users & Jobs-to-be-Done

| User | Job-to-be-Done | Today's Workaround | Pain |
|------|----------------|--------------------|------|
| **Workforce Manager** | When I plan next week's shifts, I want a schedule that meets demand at minimum cost — without me hand-checking every constraint. | Excel + judgment | Slow, suboptimal, brittle to demand changes |
| **Finance** | When I look at support-team OPEX, I want to know how much of the spend is structurally necessary vs. inefficiency. | Quarterly variance analysis | Reactive, no levers |
| **Customer Ops Lead** | When demand spikes (product launch, recall), I want to know the *minimum* staffing change to absorb it. | Add 20%, hope for the best | Wasted spend or missed SLA |

---

## The Solution

A linear-programming formulation in R that takes **demand × language × shift** as input and outputs the cost-minimizing assignment.

### Model

- **Decision variables:** Number of agents assigned per (language × shift)
- **Objective:** Minimize total daily cost
- **Constraints:** - Demand coverage per (language × shift)
  - Bilingual agents counted against either language
  - Max agents per shift (capacity)
  - Min agents per language (coverage floor)

### Key product decisions (and the tradeoffs)

| Decision | What I picked | What I rejected | Why |
|----------|---------------|-----------------|-----|
| **Linear programming over heuristics** | LP via `lpSolve` | Greedy "fill highest demand first" | LP gives a *provably* optimal solution and a defensible audit trail. For a finance-facing recommendation, "this is optimal" beats "this is what we tried." |
| **Bilingual agents as flexible capacity** | Treat them as a shared resource that can serve either language | Hard-assign to one language | Bilinguals are the cheat code: a single bilingual covers two language demand pools. Modeling that explicitly is where the savings come from. |
| **Cost as the objective, SLA as a constraint** | Minimize cost s.t. demand is met | Maximize service quality | The LP form requires picking one. Cost is the lever the workforce manager actually controls; service quality is non-negotiable (it's a constraint). |
| **R + lpSolve over Python + PuLP** | R | Python | The class context was R-first, and `lpSolve` is mature, well-documented, and outputs a clean dual / sensitivity report — useful for the "what if demand changes?" follow-up question. |

---

## Impact & Metrics

| Metric | Result |
|--------|--------|
| Daily cost saving | **$100 / day** |
| Percent reduction | **2.2%** |
| Coverage violations | 0 (all constraints satisfied) |
| Solution quality | Provably optimal (LP duality) |

**Annualized:** ~$26K / year on a single team — meaningful at scale, and the model generalizes to other regions / functions with no code changes (just new demand inputs).

---

## What I'd Build Next

| Priority | Feature | Why this, why now |
|----------|---------|-------------------|
| **P0** | **Sensitivity dashboard** | LP outputs *shadow prices* on every constraint — these tell the workforce manager which constraints are binding. Surfacing those = "if you could relax X by 1, you'd save $Y/day." That's the actually useful UI. |
| **P0** | **Demand-forecast integration** | Today the model takes demand as a static input. Plugging in a forecast (with uncertainty intervals) → a *robust* LP that hedges against forecast error. |
| **P1** | **Multi-week planning** | Single-day optimization misses spillover (an agent who works Saturday is unavailable Sunday). Multi-period MILP captures rest constraints + reduces hand-tuning. |
| **P2** | **Self-serve "what-if"** | Let the workforce manager change a constraint in a dropdown and see the new optimal cost. Turns a one-shot analysis into a tool. |

**What I would NOT build next:** A full workforce-management SaaS — that's NICE / Genesys territory. The defensible piece here is the *modeling*, not a UI.

---

## My Role

**Group 5, Operations Research course (MSBA, SCU).**

**What I personally owned:**
- Problem framing — translated the case study into a tractable LP formulation
- Bilingual-agent modeling (the move that produced the savings)
- R implementation in `Tesla_Complete_Solution.R`
- Final report write-up

---

## What I Learned

- **The formulation is the work.** Once the LP is correctly formulated, `lpSolve` returns the answer in milliseconds. 90% of the time was on identifying decision variables, constraints, and the objective.
- **Shadow prices are an under-used product surface.** The "interesting" output of an LP isn't always the optimal answer — it's *which constraints are binding*. That's actionable information for a workforce manager. Most LP outputs hide this in a footnote.
- **2.2% sounds small until you annualize and replicate.** Quantifying impact at the right denominator (per day vs. per year × per region) is a PM skill, not an OR skill.

---

## Tech Stack

| Layer | Technology |
|-------|------------|
| Modeling | Linear Programming (LP) |
| Implementation | R, `lpSolve` |
| Reporting | Sensitivity analysis (shadow prices, reduced costs) |

---

## Files

- `Tesla_Complete_Solution.R` — Full LP implementation, sensitivity analysis, results
- `Tesla_Report_Final_Complete_Group5.pdf` — Written case-study report

---

**Built by [Srinidhi Jagannathan](https://github.com/sjagannathan17)** · Santa Clara University MSBA · [LinkedIn](https://linkedin.com/in/srinidhi-jagannathan) · srinidhi.jagan11@gmail.com
