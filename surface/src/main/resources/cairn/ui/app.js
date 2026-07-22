const state = {
  route: "overview",
  overview: null,
  blocks: [],
  languages: [],
  cas: null,
  board: null,
  trust: null,
  selection: null,
};

async function api(path, opts) {
  const r = await fetch("/api/" + path, opts);
  const j = await r.json();
  if (!r.ok) throw new Error(j.error || r.statusText);
  return j;
}

function $(id) { return document.getElementById(id); }

function short(hex, n = 10) {
  if (!hex) return "—";
  return hex.length <= n * 2 ? hex : hex.slice(0, n) + "…" + hex.slice(-6);
}

function setNav(route) {
  state.route = route;
  document.querySelectorAll("#nav button").forEach((b) => {
    b.classList.toggle("active", b.dataset.route === route);
  });
  render();
}

document.querySelectorAll("#nav button").forEach((b) => {
  b.addEventListener("click", () => setNav(b.dataset.route));
});

async function loadOverview() {
  state.overview = await api("overview");
  state.blocks = await api("blocks");
  state.languages = await api("languages");
  state.cas = await api("cas/stats");
}

async function loadBoard() {
  try {
    state.board = await api("board");
  } catch (e) {
    state.board = { error: e.message, nodes: [], edges: [] };
  }
}

async function loadTrust() {
  try {
    const overview = await api("trust");
    const revocations = await api("trust/revocations");
    const delegations = await api("trust/delegations");
    state.trust = { overview, revocations, delegations };
  } catch (e) {
    state.trust = { error: e.message };
  }
}

function renderList() {
  const list = $("list");
  if (state.route === "overview") {
    const o = state.overview || {};
    list.innerHTML = `
      <div class="card">
        <h2>Node</h2>
        <div class="grid">
          <div class="stat"><div class="k">Root</div><div class="v">${esc(o.root || "")}</div></div>
          <div class="stat"><div class="k">Chain length</div><div class="v">${o.chainLength ?? 0}</div></div>
          <div class="stat"><div class="k">CAS objects</div><div class="v">${o.casObjects ?? 0}</div></div>
          <div class="stat"><div class="k">CAS bytes</div><div class="v">${o.casBytes ?? 0}</div></div>
        </div>
      </div>
      ${(o.chainLength ?? 0) === 0 ? `
        <div class="card">
          <h2>Empty node</h2>
          <p class="muted">This root has no blocks yet. Publish first, then reopen:</p>
          <pre class="view" style="min-height:auto">sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"
sbt "examples/runMain cairn.examples.Main ui &lt;node-dir&gt; 8765"</pre>
        </div>` : `<p class="muted">Open Chain to walk blocks, Board for Fact–Intent graphs, CAS for kind stats, Languages for typed surfaces.</p>`}`;
    return;
  }
  if (state.route === "chain") {
    list.innerHTML = `<h2 style="margin-top:0">Blocks</h2>` +
      (state.blocks || []).slice().reverse().map((b) => `
        <button class="list-item ${state.selection?.type === "block" && state.selection.id === b.digest ? "active" : ""}"
          data-kind="block" data-id="${b.digest}">
          <div class="title">#${b.height} <span class="pill">${esc(b.authority)}</span></div>
          <div class="meta">${short(b.digest)} · ${b.txCount} tx</div>
        </button>`).join("") || `<p class="muted">Empty chain — run a transcript to publish.</p>`;
    bindListClicks();
    return;
  }
  if (state.route === "cas") {
    const by = (state.cas && state.cas.byKind) || {};
    const kinds = Object.keys(by).sort();
    list.innerHTML = `<h2 style="margin-top:0">Artifacts by kind</h2>` +
      kinds.map((k) => `
        <div class="list-item">
          <div class="title">${esc(k)}</div>
          <div class="meta">${by[k]} objects</div>
        </div>`).join("") || `<p class="muted">CAS is empty.</p>`;
    list.innerHTML += `<p class="muted" style="margin-top:1rem">Paste a digest in the detail pane to open any artifact.</p>
      <div class="toolbar">
        <input id="digestInput" type="text" placeholder="artifact digest (hex)" style="flex:1" />
        <button class="btn primary" id="openDigest">Open</button>
      </div>`;
    $("openDigest").onclick = () => {
      const id = $("digestInput").value.trim();
      if (id) openArtifact(id);
    };
    return;
  }
  if (state.route === "board") {
    const b = state.board || {};
    if (b.error) {
      list.innerHTML = `<h2 style="margin-top:0">Search board</h2>
        <p class="muted">${esc(b.error)}</p>
        <pre class="view" style="min-height:auto">sbt "examples/runMain cairn.examples.Main transcript transcripts/search-board.cairn"</pre>`;
      return;
    }
    list.innerHTML = `<h2 style="margin-top:0">Board nodes</h2>
      <p class="muted">Digest ${short(b.digest)}</p>` +
      (b.nodes || []).map((n) => `
        <button class="list-item ${state.selection?.type === "board-node" && state.selection.id === n.name ? "active" : ""}"
          data-kind="board-node" data-id="${esc(n.name)}">
          <div class="title"><span class="pill">${esc(n.kind)}</span> ${esc(n.name)}</div>
          <div class="meta">${esc(n.text || "")}</div>
        </button>`).join("") || `<p class="muted">No nodes.</p>`;
    list.innerHTML += `<h2 style="margin-top:1.25rem">Edges</h2>` +
      (b.edges || []).map((e) => `
        <div class="list-item">
          <div class="title"><span class="pill">${esc(e.kind)}</span> ${esc(e.name)}</div>
          <div class="meta">${esc(e.from)} → ${esc(e.to)}</div>
        </div>`).join("") || `<p class="muted">No supports/spawns edges.</p>`;
    bindListClicks();
    return;
  }
  if (state.route === "trust") {
    const t = state.trust || {};
    if (t.error) {
      list.innerHTML = `<h2 style="margin-top:0">Trust</h2><p class="bad">${esc(t.error)}</p>`;
      return;
    }
    const o = t.overview || {};
    const grants = (t.revocations && t.revocations.grants) || [];
    const entries = (t.delegations && t.delegations.entries) || [];
    list.innerHTML = `
      <h2 style="margin-top:0">Capability trust</h2>
      <p class="muted">${esc(o.note || "Revocation + delegation over CAS digests (not BFT).")}</p>
      <div class="card">
        <h2>Revocations <span class="pill">${o.revokedCount ?? grants.length}</span></h2>
        ${grants.map((g) => `<div class="list-item"><div class="title">${esc(g)}</div></div>`).join("")
          || `<p class="muted">No revoked grant ids yet.</p>`}
        <div class="toolbar" style="margin-top:0.75rem">
          <input id="revokeId" type="text" placeholder="grant id" style="flex:1" />
          <button class="btn primary" id="doRevoke">Revoke</button>
        </div>
      </div>
      <div class="card">
        <h2>Delegations <span class="pill">${o.delegationCount ?? entries.length}</span></h2>
        ${entries.map((e, i) => `
          <button class="list-item ${state.selection?.type === "delegation" && state.selection.id === String(i) ? "active" : ""}"
            data-kind="delegation" data-id="${i}">
            <div class="title">${esc(e.grantor)} → ${esc(e.grantee)}</div>
            <div class="meta">${esc(e.action)} · ${esc(e.resourceKind)}/${esc(e.resourcePath)} · depth ${e.depth}</div>
          </button>`).join("") || `<p class="muted">No delegation hops recorded.</p>`}
        <div class="toolbar" style="margin-top:0.75rem; flex-wrap:wrap; gap:0.35rem">
          <input id="dlgGrantor" type="text" placeholder="grantor" style="width:6rem" />
          <input id="dlgGrantee" type="text" placeholder="grantee" style="width:6rem" />
          <input id="dlgAction" type="text" placeholder="action id" style="flex:1" value="Cas.put" />
          <button class="btn primary" id="doDelegate">Record hop</button>
        </div>
      </div>`;
    bindListClicks();
    const revokeBtn = $("doRevoke");
    if (revokeBtn) revokeBtn.onclick = async () => {
      const grantId = $("revokeId").value.trim();
      if (!grantId) return;
      try {
        await api("trust/revoke", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ grantId }),
        });
        state.trust = null;
        await loadTrust();
        render();
      } catch (e) {
        $("detail").innerHTML = `<p class="bad">${esc(e.message)}</p>`;
      }
    };
    const dlgBtn = $("doDelegate");
    if (dlgBtn) dlgBtn.onclick = async () => {
      try {
        await api("trust/delegate", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            grantor: $("dlgGrantor").value.trim(),
            grantee: $("dlgGrantee").value.trim(),
            action: $("dlgAction").value.trim() || "Cas.put",
            resourceKind: "cas",
            resourcePath: "*",
            depth: 1,
          }),
        });
        state.trust = null;
        await loadTrust();
        render();
      } catch (e) {
        $("detail").innerHTML = `<p class="bad">${esc(e.message)}</p>`;
      }
    };
    return;
  }
  if (state.route === "languages") {
    list.innerHTML = `<h2 style="margin-top:0">Loaded languages</h2>` +
      (state.languages || []).map((l) => `
        <button class="list-item ${state.selection?.type === "lang" && state.selection.id === l.name ? "active" : ""}"
          data-kind="lang" data-id="${esc(l.name)}">
          <div class="title">${esc(l.name)}</div>
          <div class="meta">${short(l.digest)} · top ${esc(l.top)}</div>
        </button>`).join("");
    bindListClicks();
  }
}

function bindListClicks() {
  $("list").querySelectorAll("button.list-item[data-kind]").forEach((el) => {
    el.addEventListener("click", () => {
      const kind = el.dataset.kind;
      const id = el.dataset.id;
      if (kind === "block") openBlock(id);
      if (kind === "lang") openLanguage(id);
      if (kind === "board-node") openBoardNode(id);
      if (kind === "delegation") openDelegation(id);
    });
  });
}

function openDelegation(idx) {
  state.selection = { type: "delegation", id: String(idx) };
  renderList();
  const entries = (state.trust && state.trust.delegations && state.trust.delegations.entries) || [];
  const e = entries[Number(idx)];
  const detail = $("detail");
  if (!e) {
    detail.innerHTML = `<p class="bad">Unknown delegation</p>`;
    return;
  }
  detail.innerHTML = `
    <div class="card">
      <h2>Delegation hop <span class="pill">capability</span></h2>
      <div class="grid">
        <div class="stat"><div class="k">Grantor</div><div class="v">${esc(e.grantor)}</div></div>
        <div class="stat"><div class="k">Grantee</div><div class="v">${esc(e.grantee)}</div></div>
        <div class="stat"><div class="k">Action</div><div class="v">${esc(e.action)}</div></div>
        <div class="stat"><div class="k">Resource</div><div class="v">${esc(e.resourceKind)} / ${esc(e.resourcePath)}</div></div>
        <div class="stat"><div class="k">Depth</div><div class="v">${e.depth}</div></div>
        <div class="stat"><div class="k">Digest</div><div class="v">${esc(e.digest || "—")}</div></div>
      </div>
    </div>
    <div class="card">
      <h2>Notes</h2>
      <p class="muted">Backed by <code>DelegationLog</code> / CAS <code>capability-delegation</code>.
      Kernel hop validation stays in <code>Authority.Delegation</code>; this view is the explorer ledger.</p>
    </div>`;
}

function esc(s) {
  return String(s)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

function openBoardNode(name) {
  state.selection = { type: "board-node", id: name };
  renderList();
  const b = state.board || {};
  const node = (b.nodes || []).find((n) => n.name === name);
  const related = (b.edges || []).filter((e) => e.from === name || e.to === name);
  const detail = $("detail");
  if (!node) {
    detail.innerHTML = `<p class="bad">Unknown board node</p>`;
    return;
  }
  detail.innerHTML = `
    <div class="card">
      <h2>${esc(node.name)} <span class="pill">${esc(node.kind)}</span></h2>
      <p>${esc(node.text || "")}</p>
      <div class="grid">
        <div class="stat"><div class="k">Board</div><div class="v">${short(b.digest)}</div></div>
      </div>
    </div>
    <div class="card">
      <h2>Incident edges</h2>
      ${related.map((e) => `
        <div class="tx">
          <span class="pill">${esc(e.kind)}</span>
          ${esc(e.from)} → ${esc(e.to)}
        </div>`).join("") || "<p class='muted'>No incident edges.</p>"}
    </div>
    <div class="card">
      <h2>Graph (read-only)</h2>
      <pre class="view">${esc(renderAsciiGraph(b))}</pre>
    </div>`;
}

function renderAsciiGraph(b) {
  const lines = [];
  for (const n of b.nodes || []) lines.push(`[${n.kind}] ${n.name}`);
  for (const e of b.edges || []) lines.push(`  ${e.from} -${e.kind}-> ${e.to}`);
  return lines.join("\n") || "(empty)";
}

async function openBlock(digest) {
  state.selection = { type: "block", id: digest };
  renderList();
  const detail = $("detail");
  detail.innerHTML = `<p class="muted">Loading block…</p>`;
  try {
    const b = await api("blocks/" + digest);
    detail.innerHTML = `
      <div class="card">
        <h2>Block #${b.height} <span class="pill">block</span></h2>
        <div class="grid">
          <div class="stat"><div class="k">Digest</div><div class="v">${esc(b.digest)}</div></div>
          <div class="stat"><div class="k">Parent</div><div class="v"><a class="link" data-parent="${esc(b.parent)}">${short(b.parent)}</a></div></div>
          <div class="stat"><div class="k">Authority</div><div class="v">${esc(b.authority)}</div></div>
          <div class="stat"><div class="k">State root</div><div class="v">${short(b.stateRoot)}</div></div>
        </div>
      </div>
      <div class="card">
        <h2>Transactions</h2>
        ${(b.txs || []).map(renderTx).join("") || "<p class='muted'>No transactions.</p>"}
      </div>
      <div class="card">
        <h2>Canon</h2>
        <pre class="view">${esc(JSON.stringify(b.canon, null, 2))}</pre>
      </div>`;
    detail.querySelectorAll("[data-artifact]").forEach((a) => {
      a.addEventListener("click", () => openArtifact(a.dataset.artifact));
    });
    const parent = detail.querySelector("[data-parent]");
    if (parent) parent.addEventListener("click", () => openBlock(parent.dataset.parent));
  } catch (e) {
    detail.innerHTML = `<p class="bad">${esc(e.message)}</p>`;
  }
}

function renderTx(tx) {
  const p = tx.payload || {};
  let body = "";
  if (tx.tag === "PublishArtifact") {
    body = `Publish <span class="pill">${esc(p.kind)}</span>
      <a class="link" data-artifact="${esc(p.valueHash)}">${short(p.valueHash)}</a>`;
  } else if (tx.tag === "SetBranchHead") {
    body = `Head <b>${esc(p.branch)}</b> →
      <span class="pill">${esc(p.kind)}</span>
      <a class="link" data-artifact="${esc(p.valueHash)}">${short(p.valueHash)}</a>`;
  } else if (tx.tag === "RegisterIdentity") {
    body = `Identity <b>${esc(p.name)}</b>`;
  } else if (tx.tag === "RecordCertificate") {
    body = `Certificate <a class="link" data-artifact="${esc(p.cert)}">${short(p.cert)}</a> (${esc(p.method)})`;
  } else {
    body = `<pre class="view" style="min-height:auto">${esc(JSON.stringify(p, null, 2))}</pre>`;
  }
  return `<div class="tx">
    <div><span class="pill">${esc(tx.tag)}</span> · signer ${esc(tx.signer)}</div>
    <div style="margin-top:0.35rem">${body}</div>
  </div>`;
}

async function openArtifact(digest) {
  state.selection = { type: "artifact", id: digest };
  const detail = $("detail");
  detail.innerHTML = `<p class="muted">Loading artifact…</p>`;
  try {
    const a = await api("artifacts/" + digest);
    const langs = (state.languages || []).map((l) => l.name);
    const defaultLang = langs.includes("search") ? "search" : (langs.includes("stlc") ? "stlc" : (langs[0] || ""));
    detail.innerHTML = `
      <div class="card">
        <h2>Artifact <span class="pill">${esc(a.kind)}</span></h2>
        <div class="grid">
          <div class="stat"><div class="k">Digest</div><div class="v">${esc(a.digest)}</div></div>
          <div class="stat"><div class="k">Key</div><div class="v">${esc(a.key)}</div></div>
          <div class="stat"><div class="k">Viewers</div><div class="v">${(a.viewers || []).map(esc).join(", ")}</div></div>
        </div>
      </div>
      <div class="card">
        <h2>Typed view</h2>
        <div class="toolbar">
          <label>Surface
            <select id="surface">
              <option value="text">text</option>
              <option value="json">json</option>
              <option value="canon">canon</option>
            </select>
          </label>
          <label>Language
            <select id="lang">
              <option value="">(auto)</option>
              ${langs.map((n) => `<option value="${esc(n)}" ${n === defaultLang ? "selected" : ""}>${esc(n)}</option>`).join("")}
            </select>
          </label>
          <button class="btn primary" id="refreshView">Render</button>
          <button class="btn" id="validateEdit">Validate edit</button>
        </div>
        <textarea id="editor" spellcheck="false"></textarea>
        <p id="editStatus" class="muted"></p>
      </div>
      <div class="card">
        <h2>Decoded</h2>
        <pre class="view" id="decoded">${esc(JSON.stringify(a.decoded, null, 2))}</pre>
      </div>`;
    const refresh = async () => {
      const surface = $("surface").value;
      const lang = $("lang").value;
      const q = new URLSearchParams({ surface });
      if (lang) q.set("lang", lang);
      const v = await api(`artifacts/${digest}/view?` + q.toString());
      $("editor").value = v.text || "";
      $("editStatus").textContent = v.editable
        ? "Editable surface — Validate runs the language parser/printer (propose only)."
        : "Read-only surface for this kind/lang pairing.";
    };
    $("refreshView").onclick = () => refresh().catch((e) => {
      $("editStatus").innerHTML = `<span class="bad">${esc(e.message)}</span>`;
    });
    $("validateEdit").onclick = async () => {
      const lang = $("lang").value;
      if (!lang) {
        $("editStatus").innerHTML = `<span class="bad">Pick a language to validate.</span>`;
        return;
      }
      try {
        const body = JSON.stringify({ lang, text: $("editor").value });
        const r = await api("parse", { method: "POST", headers: { "Content-Type": "application/json" }, body });
        $("editStatus").innerHTML = `<span class="ok">Parsed OK</span> — printer round-trip ready.`;
        if (r.printed) $("editor").value = r.printed;
      } catch (e) {
        $("editStatus").innerHTML = `<span class="bad">${esc(e.message)}</span>`;
      }
    };
    await refresh();
  } catch (e) {
    detail.innerHTML = `<p class="bad">${esc(e.message)}</p>`;
  }
}

async function openLanguage(name) {
  state.selection = { type: "lang", id: name };
  renderList();
  const lang = (state.languages || []).find((l) => l.name === name);
  const detail = $("detail");
  if (!lang) {
    detail.innerHTML = `<p class="bad">Unknown language</p>`;
    return;
  }
  detail.innerHTML = `
    <div class="card">
      <h2>Language ${esc(name)} <span class="pill">language</span></h2>
      <div class="grid">
        <div class="stat"><div class="k">Digest</div><div class="v">${esc(lang.digest)}</div></div>
        <div class="stat"><div class="k">Top</div><div class="v">${esc(lang.top)}</div></div>
      </div>
    </div>
    <div class="card">
      <h2>Constructors</h2>
      <pre class="view">${esc((lang.ctors || []).join("\n"))}</pre>
    </div>
    <div class="card">
      <h2>Scratch editor</h2>
      <p class="muted">Type a term in <b>${esc(name)}</b>; Validate uses the live grammar (propose only).</p>
      <div class="toolbar" style="margin-bottom:0.5rem">
        <input id="schemaSort" placeholder="sort (e.g. ${esc(lang.top || "Term")})" value="${esc(lang.top || "")}" style="width:10rem">
        <button class="btn" id="loadSchema">Constructors for sort</button>
        <select id="schemaCtors" style="display:none"></select>
        <button class="btn" id="insertCtor" style="display:none">Insert placeholder</button>
      </div>
      <textarea id="editor" spellcheck="false"></textarea>
      <div class="toolbar" style="margin-top:0.75rem">
        <button class="btn primary" id="validateEdit">Validate</button>
        <span id="editStatus" class="muted"></span>
      </div>
    </div>`;
  $("validateEdit").onclick = async () => {
    try {
      const body = JSON.stringify({ lang: name, text: $("editor").value });
      const r = await api("parse", { method: "POST", headers: { "Content-Type": "application/json" }, body });
      $("editStatus").innerHTML = `<span class="ok">OK</span>`;
      if (r.printed) $("editor").value = r.printed;
    } catch (e) {
      $("editStatus").innerHTML = `<span class="bad">${esc(e.message)}</span>`;
    }
  };
  // Projectional-editing proof-of-concept: schema is derived entirely from
  // the composed language's own Fragment data (GET /api/schema/<lang>/<sort>,
  // same source the LSP's cairn/schema request reads) — no per-language UI
  // code. "Insert placeholder" writes structure (constructor name + typed
  // argument slots), not hand-typed surface syntax.
  $("loadSchema").onclick = async () => {
    const sort = $("schemaSort").value.trim();
    if (!sort) return;
    try {
      const ctors = await api(`schema/${encodeURIComponent(name)}/${encodeURIComponent(sort)}`);
      const sel = $("schemaCtors");
      sel.innerHTML = ctors.map((c) =>
        `<option value='${esc(JSON.stringify(c))}'>${esc(c.name)}(${esc(c.argSorts.join(", "))})</option>`).join("");
      sel.style.display = ctors.length ? "inline-block" : "none";
      $("insertCtor").style.display = ctors.length ? "inline-block" : "none";
      $("editStatus").innerHTML = ctors.length
        ? `<span class="ok">${ctors.length} constructor(s) for ${esc(sort)}</span>`
        : `<span class="muted">no constructors for sort ${esc(sort)}</span>`;
    } catch (e) {
      $("editStatus").innerHTML = `<span class="bad">${esc(e.message)}</span>`;
    }
  };
  $("insertCtor").onclick = () => {
    const sel = $("schemaCtors");
    if (!sel.value) return;
    const c = JSON.parse(sel.value);
    const placeholder = c.argSorts.length
      ? `${c.name}(${c.argSorts.map((s) => `<${s}>`).join(", ")})`
      : c.name;
    const ta = $("editor");
    const pos = ta.selectionStart ?? ta.value.length;
    ta.value = ta.value.slice(0, pos) + placeholder + ta.value.slice(pos);
    ta.focus();
  };
}

async function render() {
  if (!state.overview) {
    try { await loadOverview(); }
    catch (e) {
      $("list").innerHTML = `<p class="bad">${esc(e.message)}</p>`;
      return;
    }
  }
  if (state.route === "board" && !state.board) {
    await loadBoard();
  }
  if (state.route === "trust" && !state.trust) {
    await loadTrust();
  }
  renderList();
  if (state.route === "overview") {
    $("detail").innerHTML = `
      <div class="card">
        <h2>Kinds in CAS</h2>
        <pre class="view">${esc(JSON.stringify((state.overview && state.overview.byKind) || {}, null, 2))}</pre>
      </div>
      <div class="card">
        <h2>How to use</h2>
        <p class="muted">Chain walks PoA blocks and transactions. Board shows a read-only
        Fact–Intent–Hint graph when a search module is in CAS. Trust lists capability
        revocations and delegation hops (CAS digest-merge — not BFT). Languages drive text
        surfaces and the validate-only editor (no silent mutation — proposals must go
        through ΔL / kernel gates).</p>
      </div>`;
  } else if (state.route === "trust" && state.trust && !state.trust.error && !state.selection) {
    const o = state.trust.overview || {};
    $("detail").innerHTML = `
      <div class="card">
        <h2>Trust surface <span class="pill">explorer</span></h2>
        <p class="muted">View and manage revocation / delegation digests backed by
        <code>RevocationLog</code> and <code>DelegationLog</code> (ReplayReplication shape).
        Studio product UI remains deferred.</p>
        <div class="grid">
          <div class="stat"><div class="k">Revoked</div><div class="v">${o.revokedCount ?? 0}</div></div>
          <div class="stat"><div class="k">Delegations</div><div class="v">${o.delegationCount ?? 0}</div></div>
        </div>
      </div>`;
  } else if (state.route === "board" && state.board && !state.board.error && !state.selection) {
    $("detail").innerHTML = `
      <div class="card">
        <h2>Fact–Intent–Hint board <span class="pill">read-only</span></h2>
        <p class="muted">Select a node for detail. Edges are <code>supports</code> / <code>spawns</code>.</p>
        <pre class="view">${esc(renderAsciiGraph(state.board))}</pre>
      </div>`;
  }
}

loadOverview().then(render).catch((e) => {
  $("list").innerHTML = `<p class="bad">${esc(e.message)}</p>`;
});
