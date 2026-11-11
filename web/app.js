"use strict";
const O = window.Optimizer;
const $ = (id) => document.getElementById(id);
const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
const passDesc = Object.fromEntries(Array.from(O.passes, (p) => [p.name, p.description]));

const S = { res: null, view: "optimize", optStep: 0, optFn: "main", runLevel: "1", runStep: 0, timer: null };

const list = (a) => Array.from(a || []);
const funcs = (prog) => list(prog);
const findFn = (prog, name) => funcs(prog).find((f) => f.name === name) || funcs(prog)[0];
const programAt = (k) => (k === 0 ? S.res.ir0 : S.res.steps[k - 1].program);
const sizeAt = (k) => (k === 0 ? S.res.size0 : S.res.steps[k - 1].size);
const finalProgram = () => programAt(S.res.steps.length);

function funcLines(f) {
  if (!f) return [];
  const out = [`define @${f.name}(${list(f.params).map((p) => "%" + p).join(", ")}) {`];
  for (const b of list(f.blocks)) {
    out.push(b.label + ":");
    for (const i of list(b.instrs)) out.push("  " + i);
    out.push("  " + b.term);
  }
  out.push("}");
  return out;
}

function diff(a, b) {
  const n = a.length, m = b.length;
  const dp = Array.from({ length: n + 1 }, () => new Int32Array(m + 1));
  for (let i = n - 1; i >= 0; i--)
    for (let j = m - 1; j >= 0; j--)
      dp[i][j] = a[i] === b[j] ? dp[i + 1][j + 1] + 1 : Math.max(dp[i + 1][j], dp[i][j + 1]);
  const ops = [];
  let i = 0, j = 0;
  while (i < n && j < m) {
    if (a[i] === b[j]) { ops.push(["same", a[i]]); i++; j++; }
    else if (dp[i + 1][j] >= dp[i][j + 1]) ops.push(["del", a[i++]]);
    else ops.push(["add", b[j++]]);
  }
  while (i < n) ops.push(["del", a[i++]]);
  while (j < m) ops.push(["add", b[j++]]);
  return ops;
}

const CH = 7.4, LH = 17, PAD = 8;
function drawCFG(el, f, opts = {}) {
  if (!f) { el.innerHTML = "<p>(function removed)</p>"; return; }
  const g = new dagre.graphlib.Graph({ multigraph: true });
  g.setGraph({ rankdir: "TB", nodesep: 30, ranksep: 38, marginx: 10, marginy: 10 });
  g.setDefaultEdgeLabel(() => ({}));
  for (const b of list(f.blocks)) {
    const lines = [b.label + ":", ...list(b.instrs), b.term];
    const w = Math.max(...lines.map((l) => l.length)) * CH + PAD * 2;
    g.setNode(b.label, { width: Math.max(w, 90), height: lines.length * LH + PAD * 2, b });
  }
  for (const b of list(f.blocks)) {
    const succs = list(b.succs);
    succs.forEach((s, k) => {
      const kind = succs.length === 2 && succs[0] !== succs[1] ? (k === 0 ? "t" : "f") : "";
      if (g.hasNode(s)) g.setEdge(b.label, s, { kind, label: kind === "t" ? "T" : kind === "f" ? "F" : "", width: 10, height: 12 }, b.label + ">" + k);
    });
  }
  dagre.layout(g);
  const { width, height } = g.graph();
  let svg = `<svg width="${width}" height="${height}" viewBox="0 0 ${width} ${height}" role="img" aria-label="Control-flow graph of @${esc(f.name)}">
    <defs><marker id="ah-${el.id}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0L10,5L0,10z" fill="var(--muted)"/></marker></defs>`;
  for (const e of g.edges()) {
    const ed = g.edge(e), pts = ed.points;
    let d = `M${pts[0].x},${pts[0].y}`;
    for (let i = 1; i < pts.length - 1; i++) {
      const mx = (pts[i].x + pts[i + 1].x) / 2, my = (pts[i].y + pts[i + 1].y) / 2;
      d += ` Q${pts[i].x},${pts[i].y} ${i === pts.length - 2 ? pts[i + 1].x + "," + pts[i + 1].y : mx + "," + my}`;
    }
    if (pts.length === 2) d += ` L${pts[1].x},${pts[1].y}`;
    const taken = opts.taken && opts.taken[0] === e.v && opts.taken[1] === e.w ? " taken" : "";
    const color = ed.kind === "t" ? "var(--true)" : ed.kind === "f" ? "var(--false)" : "var(--muted)";
    svg += `<path class="edge ${ed.kind}${taken}" d="${d}" style="color:${color}" marker-end="url(#ah-${el.id})"/>`;
    if (ed.label) svg += `<text class="elabel" x="${ed.x}" y="${ed.y + 4}" text-anchor="middle">${ed.label}</text>`;
  }
  for (const v of g.nodes()) {
    const n = g.node(v), b = n.b, x = n.x - n.width / 2, y = n.y - n.height / 2;
    const cls = ["node", opts.newBlocks && opts.newBlocks.has(v) ? "new" : "", opts.active === v ? "active" : ""].join(" ");
    svg += `<g class="${cls}"><rect x="${x}" y="${y}" width="${n.width}" height="${n.height}" rx="6"/>`;
    svg += `<text class="hdr" x="${x + PAD}" y="${y + PAD + 12}">${esc(v)}:</text>`;
    const rows = [...list(b.instrs), b.term];
    rows.forEach((line, i) => {
      const ly = y + PAD + LH * (i + 1);
      const hot = opts.active === v && opts.activeIdx === i;
      const added = opts.added && opts.added.has(v + "\u0000" + line);
      if (hot || added) svg += `<rect class="${hot ? "linehot" : "lineadd"}" x="${x + 2}" y="${ly + 2}" width="${n.width - 4}" height="${LH}" rx="3" style="stroke:none"/>`;
      svg += `<text class="${i === rows.length - 1 ? "term" : ""}" x="${x + PAD}" y="${ly + 14}">${esc(line)}</text>`;
    });
    svg += "</g>";
  }
  el.innerHTML = svg + "</svg>";
  if (opts.active) {
    const r = el.querySelector(".node.active rect");
    if (r) r.scrollIntoView({ block: "nearest", inline: "nearest" });
  }
}

function fnTabs(el, prog, current, onPick) {
  el.innerHTML = "";
  for (const f of funcs(prog)) {
    const b = document.createElement("button");
    b.textContent = "@" + f.name;
    b.setAttribute("aria-pressed", f.name === current);
    b.onclick = () => onPick(f.name);
    el.appendChild(b);
  }
}

function renderOptimize() {
  const r = S.res, k = S.optStep, n = r.steps.length;
  $("opt-slider").max = n;
  $("opt-slider").value = k;
  $("opt-label").textContent = `step ${k}/${n} · ${sizeAt(k)} instrs`;

  const tl = $("timeline");
  tl.innerHTML = "";
  for (let i = 0; i <= n; i++) {
    const c = document.createElement("button");
    const changed = i > 0 && JSON.stringify(programAt(i)) !== JSON.stringify(programAt(i - 1));
    const d = i > 0 ? sizeAt(i - 1) - sizeAt(i) : 0;
    c.className = "chip" + (changed ? " changed" : "") + (i === k ? " current" : "");
    c.innerHTML = i === 0 ? "lowered" : `R${r.steps[i - 1].round} ${esc(r.steps[i - 1].pass)}${d ? ` <span class="d">−${d}</span>` : ""}`;
    c.title = i === 0 ? "IR straight out of lowering" : changed ? "changed the IR" : "no change";
    c.onclick = () => { S.optStep = i; renderOptimize(); };
    tl.appendChild(c);
  }
  if (k === 0) $("pass-desc").textContent = "Unoptimized IR from lowering. Step through the -O1 pipeline →";
  else {
    const s = r.steps[k - 1], d = sizeAt(k - 1) - sizeAt(k);
    $("pass-desc").textContent = `round ${s.round} · ${s.pass}: ${passDesc[s.pass]}  (${d > 0 ? "removed " + d : d < 0 ? "added " + -d : "no size change"})`;
  }

  const cur = programAt(k), prev = programAt(Math.max(0, k - 1));
  if (!funcs(cur).some((f) => f.name === S.optFn)) S.optFn = "main";
  fnTabs($("opt-fns"), cur, S.optFn, (name) => { S.optFn = name; renderOptimize(); });
  const f = findFn(cur, S.optFn), pf = findFn(prev, S.optFn);

  const ops = k === 0 ? funcLines(f).map((l) => ["same", l]) : diff(funcLines(pf), funcLines(f));
  $("opt-diff").innerHTML = ops.map(([t, l]) => `<span class="${t}">${t === "add" ? "+ " : t === "del" ? "- " : "  "}${esc(l)}</span>`).join("");

  const added = new Set(), newBlocks = new Set();
  if (k > 0) {
    const oldBlocks = new Map(list(pf && pf.blocks).map((b) => [b.label, new Set([...list(b.instrs), b.term])]));
    for (const b of list(f.blocks)) {
      const old = oldBlocks.get(b.label);
      if (!old) newBlocks.add(b.label);
      for (const l of [...list(b.instrs), b.term]) if (!old || !old.has(l)) added.add(b.label + "\u0000" + l);
    }
  }
  drawCFG($("opt-cfg"), f, { added, newBlocks });
}

const trace = () => (S.runLevel === "1" ? S.res.trace1 : S.res.trace0);
const runProgram = () => (S.runLevel === "1" ? finalProgram() : S.res.ir0);

function replay(upto) {
  const prog = runProgram(), ev = list(trace().events);
  const frames = [{ fn: "main", regs: new Map() }];
  const out = [];
  let changed = null;
  for (let i = 0; i <= upto && i < ev.length; i++) {
    const e = ev[i], top = frames[frames.length - 1];
    changed = null;
    if (e.k === "set") { top.regs.set(e.reg, e.val); changed = e.reg; }
    else if (e.k === "print") out.push(e.val);
    else if (e.k === "call") {
      const callee = findFn(prog, e.callee), regs = new Map();
      list(callee.params).forEach((p, j) => regs.set(p, e.args[j]));
      frames.push({ fn: e.callee, regs });
    } else if (e.k === "ret" && frames.length > 1) frames.pop();
  }
  return { frames, out, changed };
}

function describe(e) {
  if (!e) return "";
  switch (e.k) {
    case "set": return `%${e.reg} ← ${e.val}`;
    case "print": return `print → ${e.val}`;
    case "call": return `call @${e.callee}(${list(e.args).join(", ")}) — new stack frame`;
    case "return": return "call returned";
    case "jump": return `jump → ${e.to}`;
    case "ret": return `return ${e.val} from @${e.fn}`;
  }
  return e.k;
}

function renderRun() {
  const t = trace(), ev = list(t.events), n = ev.length;
  S.runStep = Math.min(S.runStep, Math.max(0, n - 1));
  const k = S.runStep, e = ev[k];
  $("run-slider").max = Math.max(0, n - 1);
  $("run-slider").value = k;
  $("run-label").textContent = `step ${n ? k + 1 : 0}/${n}`;
  $("run-desc").textContent = e ? `@${e.fn} · ${e.block}[${e.idx}] · ${describe(e)}` : "(no steps)";
  if (k === n - 1 && t.status !== "finished") $("run-desc").textContent += `  ·  ${t.status}`;

  const st = replay(k);
  const f = e ? findFn(runProgram(), e.fn) : findFn(runProgram(), "main");
  $("run-cfg-title").textContent = `Control-flow graph · @${f.name}`;
  drawCFG($("run-cfg"), f, {
    active: e && e.block, activeIdx: e && e.idx,
    taken: e && e.k === "jump" ? [e.block, e.to] : null,
  });

  $("stack").innerHTML = st.frames.map((fr, i) => {
    const top = i === st.frames.length - 1;
    const rows = [...fr.regs].map(([r, v]) =>
      `<tr class="${top && r === st.changed ? "changed" : ""}"><td>%${esc(r)}</td><td>${v}</td></tr>`).join("");
    return `<div class="frame${top ? " top" : ""}"><h3>@${esc(fr.fn)}</h3>${rows ? `<table>${rows}</table>` : `<div class="empty">no registers yet</div>`}</div>`;
  }).join("");
  $("console").textContent = st.out.join("\n");
}

function setRunStep(k) {
  S.runStep = Math.max(0, Math.min(k, list(trace().events).length - 1));
  renderRun();
}
function togglePlay(force) {
  const playing = force === undefined ? !S.timer : force;
  clearInterval(S.timer);
  S.timer = null;
  if (playing) S.timer = setInterval(() => {
    if (S.runStep >= list(trace().events).length - 1) return togglePlay(false);
    setRunStep(S.runStep + 1);
  }, 180);
  $("run-play").textContent = S.timer ? "❚❚ Pause" : "▶ Play";
}

function renderSummary() {
  const r = S.res, s0 = r.size0, s1 = sizeAt(r.steps.length);
  const pct = (a, b) => (a ? ((100 * (a - b)) / a).toFixed(1) : "0.0");
  const match = r.run0.output === r.reference.output && r.run1.output === r.reference.output;
  const timedOut = [r.reference, r.run0, r.run1].some((x) => x.output.endsWith("timeout\n"));
  $("summary").innerHTML = `
    <div class="stat"><span>static instructions</span><b>${s0} → ${s1} (−${pct(s0, s1)}%)</b></div>
    <div class="stat"><span>executed instructions</span><b>${r.run0.steps} → ${r.run1.steps} (−${pct(r.run0.steps, r.run1.steps)}%)</b></div>
    <div class="stat"><span>passes run</span><b>${r.steps.length} (${r.steps.length ? r.steps[r.steps.length - 1].round : 0} rounds)</b></div>
    <div class="stat"><span>output vs reference interpreter</span><b class="${timedOut ? "" : match ? "good" : "bad"}">${timedOut ? "step limit hit" : match ? "✓ match" : "✗ MISMATCH"}</b></div>`;
}

function compile() {
  togglePlay(false);
  const r = O.compile($("source").value);
  $("error").hidden = r.ok;
  if (!r.ok) {
    $("error").textContent = (r.line ? `line ${r.line}: ` : "") + r.error;
    $("summary").innerHTML = "";
    return;
  }
  S.res = r;
  S.optStep = 0;
  S.runStep = 0;
  S.optFn = funcs(r.ir0)[0].name;
  renderSummary();
  render();
}

function render() {
  if (!S.res) return;
  if (S.view === "optimize") renderOptimize(); else renderRun();
}

function setView(v) {
  S.view = v;
  togglePlay(false);
  for (const b of document.querySelectorAll("nav button")) b.setAttribute("aria-selected", b.dataset.view === v);
  $("optimize-view").hidden = v !== "optimize";
  $("run-view").hidden = v !== "run";
  render();
}

const ex = list(O.examples);
$("examples").innerHTML = ex.map((e, i) => `<option value="${i}">${esc(e.name)}</option>`).join("");
const start = Math.max(0, ex.findIndex((e) => e.name === "06_gcd"));
$("examples").value = start;
$("source").value = ex[start].source;
$("examples").onchange = () => { $("source").value = ex[$("examples").value].source; compile(); };
$("compile").onclick = compile;
$("source").addEventListener("keydown", (e) => { if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) compile(); });
for (const b of document.querySelectorAll("nav button")) b.onclick = () => setView(b.dataset.view);

$("opt-slider").oninput = (e) => { S.optStep = +e.target.value; renderOptimize(); };
$("opt-prev").onclick = () => { S.optStep = Math.max(0, S.optStep - 1); renderOptimize(); };
$("opt-next").onclick = () => { S.optStep = Math.min(S.res.steps.length, S.optStep + 1); renderOptimize(); };

$("run-level").onchange = (e) => { S.runLevel = e.target.value; S.runStep = 0; togglePlay(false); renderRun(); };
$("run-slider").oninput = (e) => setRunStep(+e.target.value);
$("run-first").onclick = () => setRunStep(0);
$("run-prev").onclick = () => setRunStep(S.runStep - 1);
$("run-next").onclick = () => setRunStep(S.runStep + 1);
$("run-last").onclick = () => setRunStep(Infinity);
$("run-play").onclick = () => togglePlay();

document.addEventListener("keydown", (e) => {
  if (e.target.tagName === "TEXTAREA" || e.target.tagName === "SELECT" || !S.res) return;
  const d = e.key === "ArrowRight" ? 1 : e.key === "ArrowLeft" ? -1 : 0;
  if (!d) return;
  e.preventDefault();
  if (S.view === "optimize") { S.optStep = Math.max(0, Math.min(S.res.steps.length, S.optStep + d)); renderOptimize(); }
  else setRunStep(S.runStep + d);
});

compile();
