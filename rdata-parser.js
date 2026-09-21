// Pure-JS parser for R's serialization format (.rds single objects and
// .RData / .rda multi-object workspaces), R serialization format versions
// 2 and 3 (the versions saveRDS()/save() have written since R 2.4, and the
// only versions in real-world use today). XDR ("X\n") encoding only --
// that's what saveRDS()/save() write by default and essentially always in
// practice; ASCII ("A\n") and native-binary ("B\n") formats are detected
// and rejected with a clear message rather than silently mis-parsed.
//
// Ground-truthed byte-by-byte against real files written by R 4.3.3
// (see fixtures/ and test/parser.test.js) rather than derived purely from
// documentation, since the R-internals manual's prose is not always exact
// about bit layout.

const NA_INTEGER = -2147483648; // 0x80000000, used for NA in both LGLSXP and INTSXP

function isNaReal(hi, lo) {
  // R's NA_REAL is a specific quiet-NaN payload: high 32 bits 0x7FF00000,
  // low 32 bits 0x000007A2. Any other NaN bit pattern (e.g. plain 0/0) is
  // a genuine NaN, not NA, and R itself preserves that distinction.
  return hi === 0x7ff00000 && lo === 0x000007a2;
}

class RDataParseError extends Error {}

class ByteReader {
  constructor(buf) {
    this.view = new DataView(buf.buffer, buf.byteOffset, buf.byteLength);
    this.buf = buf;
    this.pos = 0;
  }
  i32() {
    const v = this.view.getInt32(this.pos, false);
    this.pos += 4;
    return v;
  }
  u32() {
    const v = this.view.getUint32(this.pos, false);
    this.pos += 4;
    return v;
  }
  f64() {
    const hi = this.view.getUint32(this.pos, false);
    const lo = this.view.getUint32(this.pos + 4, false);
    const v = this.view.getFloat64(this.pos, false);
    this.pos += 8;
    return { v, hi, lo };
  }
  bytes(n) {
    const b = this.buf.subarray(this.pos, this.pos + n);
    this.pos += n;
    return b;
  }
  get remaining() {
    return this.buf.length - this.pos;
  }
}

// SEXPTYPE numbers that appear in practice.
const T = {
  NILSXP: 0, SYMSXP: 1, LISTSXP: 2, CLOSXP: 3, ENVSXP: 4, PROMSXP: 5,
  LANGSXP: 6, SPECIALSXP: 7, BUILTINSXP: 8, CHARSXP: 9, LGLSXP: 10,
  INTSXP: 13, REALSXP: 14, CPLXSXP: 15, STRSXP: 16, DOTSXP: 17,
  VECSXP: 19, EXPRSXP: 20, RAWSXP: 24, S4SXP: 25,
  ALTREP_SXP: 238, NILVALUE_SXP: 254, REFSXP: 255,
};

function decodeFlags(flags) {
  const u = flags >>> 0;
  return {
    type: u & 255,
    levels: (u >>> 12) & 0xfffff,
    isObj: !!(u & (1 << 8)),
    hasAttr: !!(u & (1 << 9)),
    hasTag: !!(u & (1 << 10)),
  };
}

const textDecoderUtf8 = new TextDecoder('utf-8', { fatal: false });
const textDecoderLatin1 = new TextDecoder('windows-1252', { fatal: false });

function decodeCharBytes(bytes) {
  // R almost always writes UTF-8 (or pure-ASCII, a UTF-8 subset) content
  // for CHARSXP today, regardless of the native-encoding "levels" bit;
  // the bit matters mainly for legacy latin1-flagged strings. Try UTF-8
  // first and only fall back if it's clearly invalid.
  try {
    return textDecoderUtf8.decode(bytes);
  } catch (e) {
    return textDecoderLatin1.decode(bytes);
  }
}

// A "node" produced by the parser is one of:
//   { kind:'null' }
//   { kind:'sym', name:string }
//   { kind:'pairlist', items:[{tag,node}] }   // tag is a sym node or null
//   { kind:'char', value:string|null }         // null = NA_STRING
//   { kind:'lgl'|'int', values:Int32Array, attrs:node|null }
//   { kind:'real', values:Float64Array, naMask:Uint8Array, attrs:node|null }
//   { kind:'str', values:(string|null)[], attrs:node|null }
//   { kind:'vec', values:node[], attrs:node|null }
//   { kind:'unsupported', why:string }

function parseObject(r, refTable, depth) {
  if (depth > 5000) throw new RDataParseError('object nesting too deep');
  const flagsRaw = r.i32();
  const { type, levels, isObj, hasAttr, hasTag } = decodeFlags(flagsRaw);

  if (type === T.NILVALUE_SXP) return { kind: 'null' };

  if (type === T.REFSXP) {
    let idx = (flagsRaw >>> 8);
    if (idx === 0) idx = r.i32();
    const target = refTable[idx - 1];
    if (!target) throw new RDataParseError('bad back-reference in file');
    return target;
  }

  if (type === T.SYMSXP) {
    const printname = parseObject(r, refTable, depth + 1);
    const node = { kind: 'sym', name: printname.kind === 'char' ? printname.value : null };
    refTable.push(node);
    return node;
  }

  if (type === T.LISTSXP || type === T.LANGSXP) {
    const items = [];
    let curHasAttr = hasAttr, curHasTag = hasTag;
    let first = true;
    for (;;) {
      if (!first) {
        const f2 = r.i32();
        const d2 = decodeFlags(f2);
        if (d2.type === T.NILVALUE_SXP) break;
        if (d2.type !== T.LISTSXP && d2.type !== T.LANGSXP) {
          throw new RDataParseError('malformed pairlist in file');
        }
        curHasAttr = d2.hasAttr;
        curHasTag = d2.hasTag;
      }
      if (curHasAttr) parseObject(r, refTable, depth + 1); // node-level attrs, essentially never used; discard
      const tag = curHasTag ? parseObject(r, refTable, depth + 1) : null;
      const car = parseObject(r, refTable, depth + 1);
      items.push({ tag, node: car });
      first = false;
    }
    return { kind: 'pairlist', items };
  }

  if (type === T.CHARSXP) {
    const len = r.i32();
    if (len === -1) return { kind: 'char', value: null };
    const bytes = r.bytes(len);
    return { kind: 'char', value: decodeCharBytes(bytes) };
  }

  if (type === T.LGLSXP || type === T.INTSXP) {
    const len = readLength(r);
    const values = new Int32Array(len);
    for (let i = 0; i < len; i++) values[i] = r.i32();
    const attrs = hasAttr ? parseObject(r, refTable, depth + 1) : null;
    return { kind: type === T.LGLSXP ? 'lgl' : 'int', values, attrs };
  }

  if (type === T.REALSXP) {
    const len = readLength(r);
    const values = new Float64Array(len);
    const naMask = new Uint8Array(len);
    for (let i = 0; i < len; i++) {
      const { v, hi, lo } = r.f64();
      values[i] = v;
      if (Number.isNaN(v) && isNaReal(hi, lo)) naMask[i] = 1;
    }
    const attrs = hasAttr ? parseObject(r, refTable, depth + 1) : null;
    return { kind: 'real', values, naMask, attrs };
  }

  if (type === T.STRSXP) {
    const len = readLength(r);
    const values = new Array(len);
    for (let i = 0; i < len; i++) {
      const c = parseObject(r, refTable, depth + 1);
      values[i] = c.kind === 'char' ? c.value : null;
    }
    const attrs = hasAttr ? parseObject(r, refTable, depth + 1) : null;
    return { kind: 'str', values, attrs };
  }

  if (type === T.VECSXP || type === T.EXPRSXP) {
    const len = readLength(r);
    const values = new Array(len);
    for (let i = 0; i < len; i++) values[i] = parseObject(r, refTable, depth + 1);
    const attrs = hasAttr ? parseObject(r, refTable, depth + 1) : null;
    return { kind: 'vec', values, attrs };
  }

  if (type === T.RAWSXP) {
    const len = readLength(r);
    const values = r.bytes(len);
    const attrs = hasAttr ? parseObject(r, refTable, depth + 1) : null;
    return { kind: 'unsupported', why: 'raw vector (binary blob) — no CSV representation', attrs };
  }

  if (type === T.CPLXSXP) {
    const len = readLength(r);
    r.bytes(len * 16); // skip: 2 doubles per element
    const attrs = hasAttr ? parseObject(r, refTable, depth + 1) : null;
    return { kind: 'unsupported', why: 'complex numbers are not supported for export', attrs };
  }

  if (type === T.ALTREP_SXP) {
    const info = parseObject(r, refTable, depth + 1);
    const state = parseObject(r, refTable, depth + 1);
    const attrs = parseObject(r, refTable, depth + 1);
    return materializeAltrep(info, state, attrs);
  }

  // ENVSXP, CLOSXP, and anything else we don't need for tabular export.
  return { kind: 'unsupported', why: `R internal object type ${type} is not supported` };
}

function readLength(r) {
  const first = r.i32();
  if (first !== -1) return first >>> 0 === first ? first : first; // normal case
  // Long-vector header: -1 marker, then high 32 bits, then low 32 bits.
  const hi = r.u32();
  const lo = r.u32();
  const len = hi * 4294967296 + lo;
  if (len > 50_000_000) throw new RDataParseError('vector is too large to load in a browser tab');
  return len;
}

function pairlistGet(pairlistNode, name) {
  if (!pairlistNode || pairlistNode.kind !== 'pairlist') return null;
  for (const { tag, node } of pairlistNode.items) {
    if (tag && tag.kind === 'sym' && tag.name === name) return node;
  }
  return null;
}

function materializeAltrep(info, state, attrs) {
  // `info` is a 3-item pairlist: (class symbol . package symbol . <original type>)
  const items = info.kind === 'pairlist' ? info.items.map((it) => it.node) : [];
  const className = items[0] && items[0].kind === 'sym' ? items[0].name : null;
  const pkg = items[1] && items[1].kind === 'sym' ? items[1].name : null;

  if (pkg === 'base' && (className === 'compact_intseq' || className === 'compact_realseq')) {
    // state is REALSXP [n, start, step] regardless of whether the result
    // is materialized as integer or real.
    const n = state.values[0];
    const start = state.values[1];
    const step = state.values[2];
    if (className === 'compact_intseq') {
      const values = new Int32Array(n);
      for (let i = 0; i < n; i++) values[i] = start + i * step;
      return { kind: 'int', values, attrs };
    }
    const values = new Float64Array(n);
    const naMask = new Uint8Array(n);
    for (let i = 0; i < n; i++) values[i] = start + i * step;
    return { kind: 'real', values, naMask, attrs };
  }

  if (pkg === 'base' && className === 'deferred_string') {
    return { kind: 'unsupported', why: 'a lazily-computed string vector (R ALTREP "deferred_string") is not supported — save with `x <- as.character(x)` first' };
  }

  return { kind: 'unsupported', why: `an R-internal optimized vector (ALTREP class "${className || 'unknown'}") is not supported — save with \`x <- as.vector(x)\` (or as.integer/as.double/as.character) first` };
}

// ---- top-level file parsing ----

async function gunzip(bytes) {
  if (typeof DecompressionStream === 'undefined') {
    throw new RDataParseError('This browser does not support built-in gzip decompression (DecompressionStream). Try a recent Chrome, Edge, Firefox, or Safari.');
  }
  const ds = new DecompressionStream('gzip');
  const stream = new Blob([bytes]).stream().pipeThrough(ds);
  const buf = await new Response(stream).arrayBuffer();
  return new Uint8Array(buf);
}

function looksGzip(bytes) {
  return bytes.length > 2 && bytes[0] === 0x1f && bytes[1] === 0x8b;
}

function looksBzip2(bytes) {
  return bytes.length > 3 && bytes[0] === 0x42 && bytes[1] === 0x5a && bytes[2] === 0x68; // "BZh"
}

function looksXz(bytes) {
  return bytes.length > 6 && bytes[0] === 0xfd && bytes[1] === 0x37 && bytes[2] === 0x7a;
}

async function decompressIfNeeded(bytes) {
  if (looksGzip(bytes)) return gunzip(bytes);
  if (looksBzip2(bytes)) throw new RDataParseError('This file is bzip2-compressed (saveRDS(..., compress="bzip2")). Re-save with compress="gzip" or compress=TRUE, or with save() (which uses gzip by default), and try again.');
  if (looksXz(bytes)) throw new RDataParseError('This file is xz-compressed (saveRDS(..., compress="xz")). Re-save with compress="gzip" or compress=TRUE, or with save() (which uses gzip by default), and try again.');
  return bytes; // saveRDS(..., compress=FALSE)
}

function readHeader(r) {
  const marker = r.bytes(2);
  const markerStr = String.fromCharCode(marker[0], marker[1]);
  if (markerStr !== 'X\n') {
    if (markerStr === 'A\n' || markerStr === 'B\n') {
      throw new RDataParseError('This file uses R\'s ASCII or native-binary serialization format, not the standard XDR format saveRDS()/save() write by default. Re-save the file with the default options.');
    }
    throw new RDataParseError('This does not look like a file written by saveRDS() or save() (unrecognized header).');
  }
  const version = r.i32();
  if (version !== 2 && version !== 3) {
    throw new RDataParseError(`Unsupported R serialization format version ${version}. This tool supports versions 2 and 3 (everything saveRDS()/save() have written since R 2.4).`);
  }
  r.i32(); // writer version, informational only
  r.i32(); // min reader version, informational only
  if (version === 3) {
    const encLen = r.i32();
    r.bytes(encLen); // native encoding name, informational only (we always try UTF-8)
  }
  return { version };
}

// Returns { kind: 'rds', root: node } or { kind: 'rdata', vars: [{name, node}] }
async function parseRDataFile(arrayBuffer) {
  let bytes = new Uint8Array(arrayBuffer);
  bytes = await decompressIfNeeded(bytes);

  let isMulti = false;
  if (bytes.length > 5 && bytes[0] === 0x52 && bytes[1] === 0x44) {
    // "RD" magic: an .RData/.rda workspace (RDX2/RDX3/RDA2...). Skip past
    // the magic line (up to and including its newline), then the body is
    // the normal XDR header followed by a pairlist of (name . value).
    let nl = 0;
    while (nl < bytes.length && bytes[nl] !== 0x0a) nl++;
    bytes = bytes.subarray(nl + 1);
    isMulti = true;
  }

  const r = new ByteReader(bytes);
  readHeader(r);
  const refTable = [];
  const root = parseObject(r, refTable, 0);

  if (isMulti) {
    const vars = [];
    if (root.kind === 'pairlist') {
      for (const { tag, node } of root.items) {
        vars.push({ name: tag && tag.kind === 'sym' ? tag.name : '(unnamed)', node });
      }
    }
    return { kind: 'rdata', vars };
  }
  return { kind: 'rds', root };
}

// ---- turning a parsed node into a table of "variables" for the UI,
// mirroring the mat-to-csv / npy-to-csv variable-picker pattern ----

function classOf(node) {
  const cls = pairlistGet(node.attrs, 'class');
  if (cls && cls.kind === 'str' && cls.values.length) return cls.values;
  return null;
}

function isDataFrame(node) {
  if (!node || node.kind !== 'vec') return false;
  const cls = classOf(node);
  return !!cls && cls.includes('data.frame');
}

function namesOf(node) {
  const nm = pairlistGet(node.attrs, 'names');
  if (nm && nm.kind === 'str') return nm.values;
  return null;
}

function dataFrameRowCount(node) {
  const rn = pairlistGet(node.attrs, 'row.names');
  if (rn) {
    if (rn.kind === 'int' && rn.values.length === 2 && rn.values[0] === NA_INTEGER) {
      return Math.abs(rn.values[1]);
    }
    if (rn.kind === 'int' || rn.kind === 'lgl') return rn.values.length;
    if (rn.kind === 'str') return rn.values.length;
  }
  return node.values.length ? columnLength(node.values[0]) : 0;
}

function columnLength(node) {
  if (!node) return 0;
  if (node.kind === 'int' || node.kind === 'lgl') return node.values.length;
  if (node.kind === 'real') return node.values.length;
  if (node.kind === 'str') return node.values.length;
  return 0;
}

function isFactor(node) {
  const cls = classOf(node);
  return !!cls && cls.includes('factor') && (node.kind === 'int' || node.kind === 'lgl');
}

function factorLevels(node) {
  const lv = pairlistGet(node.attrs, 'levels');
  return lv && lv.kind === 'str' ? lv.values : null;
}

// A "column" is rendered to an array of strings (CSV-ready, NA -> '').
function columnToStrings(node) {
  if (!node) return [];
  if (isFactor(node)) {
    const levels = factorLevels(node) || [];
    return Array.from(node.values, (code) => (code === NA_INTEGER || code < 1 ? '' : (levels[code - 1] ?? '')));
  }
  if (node.kind === 'int') {
    return Array.from(node.values, (v) => (v === NA_INTEGER ? '' : String(v)));
  }
  if (node.kind === 'lgl') {
    return Array.from(node.values, (v) => (v === NA_INTEGER ? '' : v ? 'TRUE' : 'FALSE'));
  }
  if (node.kind === 'real') {
    const out = new Array(node.values.length);
    for (let i = 0; i < node.values.length; i++) {
      out[i] = node.naMask[i] ? '' : formatReal(node.values[i]);
    }
    return out;
  }
  if (node.kind === 'str') {
    return node.values.map((v) => (v === null ? '' : v));
  }
  return [];
}

function formatReal(v) {
  if (Number.isNaN(v)) return 'NaN';
  if (v === Infinity) return 'Inf';
  if (v === -Infinity) return '-Inf';
  if (Number.isInteger(v) && Math.abs(v) < 1e15) return String(v);
  return String(v);
}

// dim attribute (matrices): returns [nrow, ncol] or null
function dimOf(node) {
  const d = pairlistGet(node.attrs, 'dim');
  if (d && (d.kind === 'int' || d.kind === 'lgl') && d.values.length === 2) {
    return [d.values[0], d.values[1]];
  }
  return null;
}

function describeVariable(name, node) {
  if (isDataFrame(node)) {
    return { name, type: 'data.frame', shape: `${dataFrameRowCount(node)} rows x ${node.values.length} cols`, exportable: true };
  }
  const dim = dimOf(node);
  if (dim && (node.kind === 'int' || node.kind === 'real' || node.kind === 'lgl' || node.kind === 'str')) {
    return { name, type: `matrix (${node.kind})`, shape: `${dim[0]} x ${dim[1]}`, exportable: true };
  }
  if (node.kind === 'int' || node.kind === 'real' || node.kind === 'lgl' || node.kind === 'str') {
    const factor = isFactor(node);
    return { name, type: factor ? 'factor' : node.kind, shape: `${node.values.length} elements`, exportable: true };
  }
  if (node.kind === 'vec') {
    return { name, type: 'list', shape: `${node.values.length} elements`, exportable: false };
  }
  if (node.kind === 'null') {
    return { name, type: 'NULL', shape: '', exportable: false };
  }
  return { name, type: 'unsupported', shape: node.why || '', exportable: false };
}

// Turns a variable's node into { header:[...], rows:[[...]] } for preview/export.
function variableToTable(node) {
  if (isDataFrame(node)) {
    const names = namesOf(node) || node.values.map((_, i) => `V${i + 1}`);
    const cols = node.values.map(columnToStrings);
    const nrow = dataFrameRowCount(node);
    const rows = [];
    for (let i = 0; i < nrow; i++) rows.push(cols.map((c) => c[i] ?? ''));
    return { header: names, rows };
  }
  const dim = dimOf(node);
  if (dim && (node.kind === 'int' || node.kind === 'real' || node.kind === 'lgl' || node.kind === 'str')) {
    const [nrow, ncol] = dim;
    const flat = columnToStrings(node); // column-major, matches R
    const header = Array.from({ length: ncol }, (_, c) => `V${c + 1}`);
    const rows = [];
    for (let ri = 0; ri < nrow; ri++) {
      const row = [];
      for (let ci = 0; ci < ncol; ci++) row.push(flat[ri + ci * nrow] ?? '');
      rows.push(row);
    }
    return { header, rows };
  }
  if (node.kind === 'int' || node.kind === 'real' || node.kind === 'lgl' || node.kind === 'str') {
    const col = columnToStrings(node);
    return { header: ['value'], rows: col.map((v) => [v]) };
  }
  throw new RDataParseError('This variable cannot be exported to CSV.');
}

function csvEscape(s) {
  if (s === '' || s === undefined || s === null) return '';
  if (/[",\n\r]/.test(s)) return '"' + s.replace(/"/g, '""') + '"';
  return s;
}

function tableToCsv({ header, rows }) {
  const lines = [header.map(csvEscape).join(',')];
  for (const row of rows) lines.push(row.map(csvEscape).join(','));
  return lines.join('\r\n') + '\r\n';
}

// Node.js / browser export
const RDataParser = {
  parseRDataFile,
  describeVariable,
  variableToTable,
  tableToCsv,
  isDataFrame,
  namesOf,
  RDataParseError,
};

if (typeof module !== 'undefined' && module.exports) {
  module.exports = RDataParser;
} else {
  window.RDataParser = RDataParser;
}
