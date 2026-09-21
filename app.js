const $ = (id) => document.getElementById(id);

let currentVars = []; // [{name, node, desc}]
let selectedIndex = -1;

function setStatus(msg, isError) {
  const el = $('status');
  el.textContent = msg || '';
  el.classList.toggle('error', !!isError);
}

function escapeHtml(s) {
  return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

async function handleFile(file) {
  $('fname').textContent = file.name;
  setStatus('Reading and parsing…');
  $('varsCard').style.display = 'none';
  $('previewCard').style.display = 'none';
  selectedIndex = -1;
  try {
    const buf = await file.arrayBuffer();
    const result = await RDataParser.parseRDataFile(buf);
    if (result.kind === 'rdata') {
      currentVars = result.vars.map(({ name, node }) => ({ name, node, desc: RDataParser.describeVariable(name, node) }));
      setStatus(`Loaded ${currentVars.length} object(s) from this .RData workspace.`);
    } else {
      // Single object from saveRDS(). If it's a plain list (not a
      // data.frame), expose its top-level elements the same way, so
      // both file kinds share one variable-picker UI. Otherwise treat
      // the whole object as one variable.
      const root = result.root;
      if (root.kind === 'vec' && !RDataParser.isDataFrame(root)) {
        const names = RDataParser.namesOf(root) || root.values.map((_, i) => `[[${i + 1}]]`);
        currentVars = root.values.map((node, i) => ({ name: names[i] || `[[${i + 1}]]`, node, desc: RDataParser.describeVariable(names[i] || `[[${i + 1}]]`, node) }));
        setStatus(`Loaded a list of ${currentVars.length} object(s) from this .rds file.`);
      } else {
        const base = (file.name || 'object').replace(/\.rds$/i, '');
        const desc = RDataParser.describeVariable(base, root);
        currentVars = [{ name: base, node: root, desc }];
        setStatus('Loaded 1 object from this .rds file.');
      }
    }
    renderVars();
  } catch (err) {
    setStatus((err && err.message) || String(err), true);
  }
}

function renderVars() {
  const body = $('varsBody');
  const card = $('varsCard');
  if (!currentVars.length) {
    card.style.display = 'none';
    return;
  }
  card.style.display = '';
  body.innerHTML = currentVars
    .map((v, i) => {
      const cls = v.desc.exportable ? 'var-row' : 'var-row errored';
      return `<tr class="${cls}" data-idx="${i}"><td>${escapeHtml(v.name)}</td><td>${escapeHtml(v.desc.type)}</td><td>${escapeHtml(v.desc.shape)}</td></tr>`;
    })
    .join('');
  body.querySelectorAll('tr[data-idx]').forEach((tr) => {
    tr.addEventListener('click', () => selectVar(Number(tr.dataset.idx)));
  });
  // Auto-select the first exportable variable so a plain .rds vector or a
  // single-data.frame workspace previews immediately without an extra click.
  const firstExportable = currentVars.findIndex((v) => v.desc.exportable);
  if (firstExportable !== -1) selectVar(firstExportable);
}

function selectVar(i) {
  const v = currentVars[i];
  if (!v || !v.desc.exportable) return;
  selectedIndex = i;
  document.querySelectorAll('#varsBody tr').forEach((tr) => tr.classList.toggle('selected', Number(tr.dataset.idx) === i));

  let table;
  try {
    table = RDataParser.variableToTable(v.node);
  } catch (err) {
    setStatus((err && err.message) || String(err), true);
    return;
  }

  $('previewCard').style.display = '';
  $('previewHeading').textContent = `Preview — ${v.name}`;
  const MAX_ROWS = 200;
  const shown = table.rows.slice(0, MAX_ROWS);
  const thead = `<thead><tr>${table.header.map((h) => `<th>${escapeHtml(h)}</th>`).join('')}</tr></thead>`;
  const tbody = `<tbody>${shown.map((row) => `<tr>${row.map((c) => `<td>${escapeHtml(c)}</td>`).join('')}</tr>`).join('')}</tbody>`;
  $('previewTable').innerHTML = thead + tbody;
  $('truncNote').textContent = table.rows.length > MAX_ROWS ? `Showing the first ${MAX_ROWS} of ${table.rows.length} rows. The full data exports to CSV regardless.` : '';
  $('exportStatus').textContent = '';
  $('exportStatus').classList.remove('warn');
}

function exportCsv() {
  if (selectedIndex === -1) return;
  const v = currentVars[selectedIndex];
  let table;
  try {
    table = RDataParser.variableToTable(v.node);
  } catch (err) {
    $('exportStatus').textContent = (err && err.message) || String(err);
    $('exportStatus').classList.add('warn');
    return;
  }
  const csv = RDataParser.tableToCsv(table);
  const blob = new Blob([csv], { type: 'text/csv' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  const safeName = (v.name || 'data').replace(/[^a-z0-9_.-]+/gi, '_');
  a.href = url;
  a.download = `${safeName}.csv`;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 2000);
  $('exportStatus').textContent = `Exported ${table.rows.length} row(s) to ${safeName}.csv`;
}

function bindDrop() {
  const dz = $('dropzone');
  const input = $('fileInput');
  const setDrag = (on) => dz.classList.toggle('drag', on);
  ['dragenter', 'dragover'].forEach((ev) => dz.addEventListener(ev, (e) => { e.preventDefault(); setDrag(true); }));
  ['dragleave', 'drop'].forEach((ev) => dz.addEventListener(ev, (e) => { e.preventDefault(); setDrag(false); }));
  dz.addEventListener('drop', (e) => {
    const file = e.dataTransfer.files && e.dataTransfer.files[0];
    if (file) handleFile(file);
  });
  dz.addEventListener('click', () => input.click());
  dz.addEventListener('keydown', (e) => {
    if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); input.click(); }
  });
  input.addEventListener('change', () => {
    if (input.files && input.files[0]) handleFile(input.files[0]);
    input.value = '';
  });
}

bindDrop();
$('exportBtn').addEventListener('click', exportCsv);
