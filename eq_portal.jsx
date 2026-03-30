import { useState, useCallback, useMemo, useRef, useEffect } from "react";

// ─────────────────────────────────────────────────────────────────────────────
// DATA LOGIC
// ─────────────────────────────────────────────────────────────────────────────

const parseATTEXT = (text) => {
  const lines = text.trim().split("\n").filter((l) => l.trim());
  return lines
    .map((line) => {
      const cols =
        line.match(/'([^']*)'/g)?.map((v) => v.replace(/'/g, "")) || [];
      return {
        blName: cols[0] || "",
        name:   cols[1] || "",
        eqNum:  cols[2] || "",
        type:   cols[3] || "",   // present if ATTEXT template includes TYPE
      };
    })
    .filter((r) => r.blName);
};

// Mirror of get-block-type LISP function
const getType = (blkName) => {
  const n = blkName.toUpperCase();
  const w = (...pats) =>
    pats.some((pat) =>
      new RegExp("^" + pat.replace(/[-[\]{}()+?.,\\^$|#\s]/g, "\\$&").replace(/\*/g, ".*") + "$").test(n)
    );
  if (w("LAC-PO-DR-*", "LAC-POB-DR-*"))            return "Drive - Poly-O";
  if (w("*PR-OS-DRIVE*"))                           return "Drive - Pallet Roller";
  if (w("LAC-PO-P*", "LAC-POB-P*", "LAC-POM-*"))  return "Conveyor - Poly-O";
  if (w("LAC-PR-P*"))                               return "Conveyor - Pallet Roller";
  if (w("LAC-PV*"))                                 return "Conveyor - Poly-V";
  if (w("LAC-24V-TRANSFER*", "TONG CONVEYOR*"))     return "Conveyor";
  if (w("IRB*", "KR_*"))                            return "Robot";
  if (w("*GRIPPER*", "*TOOL*"))                     return "End Effector";
  if (w("*RETRO-REFLECTIVE*", "*DIFFUSED PROXY*", "*DIFFUSED*PROXY*", "*LIGHT_CURTAIN*")) return "Sensor";
  if (w("*PUSHER*", "*BLADE STOP*", "*DIVERT*", "*SEPARATION STATION*", "*SCISSOR LIFT*")) return "Actuator";
  if (w("INFILLPLATE*", "PALLET*", "COMPACTOR*"))   return "Ancillary";
  return "Other";
};

const resolveType = (row) => row.type || getType(row.blName);

const buildBOMWithType = (rows) => {
  const map = {};
  rows.forEach((r) => {
    const type = resolveType(r);
    const key  = r.name || r.blName;
    if (!map[key]) {
      map[key] = { name: key, blName: r.blName, type, qty: 0, eqNums: [] };
    }
    map[key].qty++;
    const isDefaultEq = r.eqNum && r.eqNum !== r.blName + "-001";
    if (isDefaultEq) map[key].eqNums.push(r.eqNum);
  });
  return Object.values(map);
};

const zoneOf = (eqNums) => {
  const first = eqNums[0];
  return first ? first.split(".")[0] : "—";
};

const parseEqNum = (eq) => eq.split(".").map(Number);

const buildTopology = (rows) => {
  const items = rows
    .filter((r) => r.eqNum && r.eqNum !== r.blName + "-001")
    .map((r) => ({ ...r, parts: parseEqNum(r.eqNum) }))
    .sort((a, b) => {
      for (let i = 0; i < Math.max(a.parts.length, b.parts.length); i++) {
        const diff = (a.parts[i] || 0) - (b.parts[i] || 0);
        if (diff !== 0) return diff;
      }
      return 0;
    });
  const root = { children: {} };
  items.forEach((item) => {
    let node = root;
    item.parts.forEach((p, i) => {
      if (!node.children[p]) node.children[p] = { children: {}, items: [] };
      if (i === item.parts.length - 1) node.children[p].items.push(item);
      node = node.children[p];
    });
  });
  return { tree: root, items };
};

const downloadCSV = (bom, visibleCols) => {
  const cols = [
    visibleCols.type        && "Type",
    visibleCols.description && "Description",
    visibleCols.qty         && "Qty",
    visibleCols.zone        && "Zone",
    visibleCols.eqNums      && "EQ Numbers",
  ].filter(Boolean);

  const rows = bom.map((item) =>
    [
      visibleCols.type        && item.type,
      visibleCols.description && item.name,
      visibleCols.qty         && item.qty,
      visibleCols.zone        && zoneOf(item.eqNums),
      visibleCols.eqNums      && item.eqNums.join(", "),
    ]
      .filter((v) => v !== false)
      .map((v) => `"${String(v ?? "").replace(/"/g, '""')}"`)
      .join(",")
  );

  const csv = [cols.join(","), ...rows].join("\n");
  const blob = new Blob([csv], { type: "text/csv" });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement("a");
  a.href     = url;
  a.download = "equipment-list.csv";
  a.click();
  URL.revokeObjectURL(url);
};

// ─────────────────────────────────────────────────────────────────────────────
// DESIGN TOKENS
// ─────────────────────────────────────────────────────────────────────────────
const C = {
  navy:      "#1e3a8a",
  navyDark:  "#0d1f4f",
  white:     "#ffffff",
  offWhite:  "#f5f6f8",
  rowAlt:    "#f0f4ff",
  border:    "#e2e8f0",
  text:      "#111827",
  textMuted: "#6b7280",
  textLight: "#9ca3af",
  purple:    "#7c3aed",
  blue:      "#2563eb",
  green:     "#16a34a",
};

const TYPE_COLOR = {
  "Conveyor - Poly-O":      "#2563eb",
  "Conveyor - Pallet Roller":"#0891b2",
  "Conveyor - Poly-V":      "#7c3aed",
  "Conveyor":               "#6366f1",
  "Drive - Poly-O":         "#ea580c",
  "Drive - Pallet Roller":  "#dc2626",
  "Robot":                  "#16a34a",
  "End Effector":           "#ca8a04",
  "Sensor":                 "#db2777",
  "Actuator":               "#9333ea",
  "Ancillary":              "#64748b",
  "Other":                  "#9ca3af",
};

const typeColor = (type) => TYPE_COLOR[type] || "#9ca3af";

// ─────────────────────────────────────────────────────────────────────────────
// MOTIONTECH LOGO
// ─────────────────────────────────────────────────────────────────────────────
const MTLogo = ({ size = 1 }) => (
  <div style={{ display: "flex", alignItems: "center", gap: 7 * size }}>
    <span style={{ fontSize: 20 * size, fontWeight: 700, color: C.text, letterSpacing: "-0.02em" }}>
      MotionTech
    </span>
    <svg width={28 * size} height={28 * size} viewBox="0 0 36 36" fill="none" aria-hidden="true">
      <defs>
        <linearGradient id={`mt_ring_${size}`} x1="4" y1="4" x2="32" y2="32" gradientUnits="userSpaceOnUse">
          <stop offset="0%" stopColor={C.purple} />
          <stop offset="100%" stopColor={C.blue} />
        </linearGradient>
      </defs>
      <circle cx="18" cy="18" r="13" stroke={`url(#mt_ring_${size})`} strokeWidth="4" fill="none" strokeLinecap="round" />
    </svg>
  </div>
);

// ─────────────────────────────────────────────────────────────────────────────
// TOPOLOGY SUB-COMPONENTS  (unchanged)
// ─────────────────────────────────────────────────────────────────────────────
const EqRow = ({ eqNum, name }) => (
  <div style={{ display: "flex", alignItems: "center", gap: 14, padding: "7px 0", borderBottom: `1px solid ${C.border}` }}>
    <span style={{ fontFamily: "monospace", fontSize: 12, color: C.navy, fontWeight: 700, width: 90, flexShrink: 0 }}>{eqNum}</span>
    <span style={{ fontSize: 13, color: C.text }}>{name}</span>
  </div>
);

const TopologyNode = ({ node, depth }) => {
  const [open, setOpen] = useState(true);
  const keys  = Object.keys(node.children).sort((a, b) => Number(a) - Number(b));
  const items = node.items || [];
  return (
    <div style={{ marginLeft: depth * 18 }}>
      {items.map((item, i) => <EqRow key={i} eqNum={item.eqNum} name={item.name} />)}
      {keys.length > 0 && (
        <>
          <button onClick={() => setOpen((o) => !o)}
            style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 11, color: C.textMuted, background: "none", border: "none", cursor: "pointer", padding: "4px 0", marginTop: 2, fontFamily: "inherit" }}>
            <span>{open ? "▾" : "▸"}</span>
            <span>{keys.length} sub-item{keys.length !== 1 ? "s" : ""}</span>
          </button>
          {open && keys.map((k) => <TopologyNode key={k} node={node.children[k]} depth={depth + 1} />)}
        </>
      )}
    </div>
  );
};

const TopologyView = ({ rows }) => {
  const { tree, items } = buildTopology(rows);
  const lines = Object.keys(tree.children).sort((a, b) => Number(a) - Number(b));
  const [openLines, setOpenLines] = useState({});

  if (items.length === 0)
    return (
      <div style={{ textAlign: "center", padding: 36, color: C.textMuted, fontSize: 13, background: C.offWhite, borderRadius: 6, border: `1px solid ${C.border}` }}>
        No EQ_NUM values assigned yet. Run the script, assign equipment numbers via EATTEDIT, then re-export.
      </div>
    );

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
      {lines.map((lineKey) => {
        const lineNode = tree.children[lineKey];
        const isOpen   = openLines[lineKey] !== false;
        const directCount = lineNode.items?.length || 0;
        return (
          <div key={lineKey} style={{ border: `1px solid ${C.border}`, borderRadius: 8, overflow: "hidden" }}>
            <button onClick={() => setOpenLines((s) => ({ ...s, [lineKey]: !isOpen }))}
              style={{ width: "100%", display: "flex", alignItems: "center", gap: 10, padding: "11px 18px", background: C.navy, color: C.white, border: "none", cursor: "pointer", textAlign: "left", fontFamily: "inherit" }}>
              <span style={{ fontSize: 12 }}>{isOpen ? "▾" : "▸"}</span>
              <span style={{ fontWeight: 700, fontSize: 13, letterSpacing: "0.06em", textTransform: "uppercase" }}>Line {lineKey}</span>
              <span style={{ marginLeft: "auto", fontSize: 11, opacity: 0.65 }}>{directCount} direct item{directCount !== 1 ? "s" : ""}</span>
            </button>
            {isOpen && (
              <div style={{ padding: "14px 18px", background: C.white }}>
                {(lineNode.items || []).map((item, i) => <EqRow key={i} eqNum={item.eqNum} name={item.name} />)}
                {Object.keys(lineNode.children).sort((a, b) => Number(a) - Number(b)).map((k) => (
                  <TopologyNode key={k} node={lineNode.children[k]} depth={1} />
                ))}
              </div>
            )}
          </div>
        );
      })}
    </div>
  );
};

// ─────────────────────────────────────────────────────────────────────────────
// DROPDOWN  (reusable filter/column menu)
// ─────────────────────────────────────────────────────────────────────────────
const Dropdown = ({ open, onClose, children, style = {} }) => {
  const ref = useRef(null);
  useEffect(() => {
    if (!open) return;
    const handler = (e) => { if (ref.current && !ref.current.contains(e.target)) onClose(); };
    document.addEventListener("mousedown", handler);
    return () => document.removeEventListener("mousedown", handler);
  }, [open, onClose]);
  if (!open) return null;
  return (
    <div ref={ref} style={{
      position: "absolute", zIndex: 200, background: C.white,
      border: `1px solid ${C.border}`, borderRadius: 8,
      boxShadow: "0 4px 16px rgba(0,0,0,0.12)", minWidth: 200,
      padding: "8px 0", ...style,
    }}>
      {children}
    </div>
  );
};

// ─────────────────────────────────────────────────────────────────────────────
// BOM VIEW
// ─────────────────────────────────────────────────────────────────────────────
const DEFAULT_COLS = { type: true, description: true, qty: true, zone: false, eqNums: false };

const BOMView = ({ rows }) => {
  const allBom = useMemo(() => buildBOMWithType(rows), [rows]);

  // ── state ──
  const [sortCol, setSortCol]             = useState("type");
  const [sortDir, setSortDir]             = useState("asc");
  const [typeFilters, setTypeFilters]     = useState(new Set());    // empty = all
  const [textFilter, setTextFilter]       = useState("");
  const [flagged, setFlagged]             = useState(new Set());
  const [showFlaggedOnly, setShowFlaggedOnly] = useState(false);
  const [groupBy, setGroupBy]             = useState("none");       // "none"|"type"|"zone"
  const [compact, setCompact]             = useState(true);
  const [visibleCols, setVisibleCols]     = useState(DEFAULT_COLS);
  const [typeMenuOpen, setTypeMenuOpen]   = useState(false);
  const [colMenuOpen, setColMenuOpen]     = useState(false);

  // ── derived ──
  const allTypes = useMemo(() => [...new Set(allBom.map((r) => r.type))].sort(), [allBom]);

  const filtered = useMemo(() => {
    let data = allBom;
    if (typeFilters.size) data = data.filter((r) => typeFilters.has(r.type));
    if (textFilter)       data = data.filter((r) => r.name.toLowerCase().includes(textFilter.toLowerCase()));
    if (showFlaggedOnly)  data = data.filter((r) => flagged.has(r.name));
    return data;
  }, [allBom, typeFilters, textFilter, showFlaggedOnly, flagged]);

  const sorted = useMemo(() => {
    const dir = sortDir === "asc" ? 1 : -1;
    return [...filtered].sort((a, b) => {
      if (sortCol === "qty")  return dir * (a.qty - b.qty);
      if (sortCol === "zone") return dir * (zoneOf(a.eqNums).localeCompare(zoneOf(b.eqNums)));
      return dir * String(a[sortCol] ?? "").localeCompare(String(b[sortCol] ?? ""));
    });
  }, [filtered, sortCol, sortDir]);

  const typeSummary = useMemo(() => {
    const map = {};
    filtered.forEach((r) => { map[r.type] = (map[r.type] || 0) + r.qty; });
    return Object.entries(map).sort((a, b) => b[1] - a[1]);
  }, [filtered]);

  const grouped = useMemo(() => {
    if (groupBy === "none") return [{ key: null, items: sorted }];
    const map = {};
    sorted.forEach((r) => {
      const key = groupBy === "type" ? r.type : zoneOf(r.eqNums);
      if (!map[key]) map[key] = [];
      map[key].push(r);
    });
    return Object.entries(map).sort(([a], [b]) => a.localeCompare(b)).map(([key, items]) => ({ key, items }));
  }, [sorted, groupBy]);

  const handleSort = (col) => {
    if (sortCol === col) setSortDir((d) => (d === "asc" ? "desc" : "asc"));
    else { setSortCol(col); setSortDir("asc"); }
  };

  const toggleTypeFilter = (type) => {
    setTypeFilters((prev) => {
      const next = new Set(prev);
      next.has(type) ? next.delete(type) : next.add(type);
      return next;
    });
  };

  const toggleFlag = (name) => {
    setFlagged((prev) => {
      const next = new Set(prev);
      next.has(name) ? next.delete(name) : next.add(name);
      return next;
    });
  };

  const toggleCol = (col) =>
    setVisibleCols((prev) => ({ ...prev, [col]: !prev[col] }));

  // ── col definitions ──
  const COL_DEFS = [
    { key: "type",        label: "Type",        filterable: true },
    { key: "description", label: "Description", filterable: false },
    { key: "qty",         label: "Qty",         filterable: false },
    { key: "zone",        label: "Zone",        filterable: false },
    { key: "eqNums",      label: "EQ Numbers",  filterable: false },
  ];

  const activeCols = compact
    ? COL_DEFS.filter((c) => ["type", "description", "qty"].includes(c.key))
    : COL_DEFS.filter((c) => visibleCols[c.key]);

  const SortIcon = ({ col }) => {
    if (sortCol !== col) return <span style={{ opacity: 0.3, fontSize: 10 }}>⇅</span>;
    return <span style={{ fontSize: 10 }}>{sortDir === "asc" ? "▲" : "▼"}</span>;
  };

  const thStyle = (align = "left") => ({
    textAlign: align,
    padding: compact ? "9px 14px" : "11px 18px",
    fontWeight: 600,
    letterSpacing: "0.07em",
    textTransform: "uppercase",
    fontSize: 11,
    userSelect: "none",
    whiteSpace: "nowrap",
  });

  const tdStyle = (align = "left") => ({
    padding: compact ? "6px 14px" : "8px 18px",
    textAlign: align,
    borderBottom: `1px solid ${C.border}`,
    fontSize: compact ? 12 : 13,
  });

  return (
    <div>
      {/* ── SUMMARY DASHBOARD ── */}
      <div style={{ padding: "16px 18px", borderBottom: `1px solid ${C.border}`, background: C.offWhite }}>
        <div style={{ fontSize: 10, fontWeight: 700, letterSpacing: "0.08em", textTransform: "uppercase", color: C.textMuted, marginBottom: 10 }}>
          Equipment by Type
        </div>
        <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
          {typeSummary.map(([type, total]) => {
            const active = typeFilters.has(type);
            const color  = typeColor(type);
            return (
              <button
                key={type}
                onClick={() => toggleTypeFilter(type)}
                style={{
                  display: "flex", alignItems: "center", gap: 6,
                  padding: "4px 10px 4px 7px",
                  borderRadius: 20,
                  border: `1.5px solid ${active ? color : C.border}`,
                  background: active ? color + "18" : C.white,
                  cursor: "pointer", fontFamily: "inherit",
                  fontSize: 11, fontWeight: active ? 700 : 500,
                  color: active ? color : C.text,
                  transition: "all 0.12s",
                }}
              >
                <span style={{ width: 8, height: 8, borderRadius: "50%", background: color, flexShrink: 0 }} />
                <span>{type}</span>
                <span style={{ marginLeft: 2, fontWeight: 700, color: active ? color : C.textMuted }}>{total}</span>
              </button>
            );
          })}
        </div>
      </div>

      {/* ── TOOLBAR ── */}
      <div style={{
        padding: "10px 18px", borderBottom: `1px solid ${C.border}`,
        display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap",
        background: C.white,
      }}>
        {/* Text search */}
        <input
          value={textFilter}
          onChange={(e) => setTextFilter(e.target.value)}
          placeholder="Search description…"
          style={{
            border: `1px solid ${C.border}`, borderRadius: 6, padding: "5px 10px",
            fontSize: 12, fontFamily: "inherit", outline: "none", width: 200,
            color: C.text,
          }}
        />

        {/* Group by */}
        <select
          value={groupBy}
          onChange={(e) => setGroupBy(e.target.value)}
          style={{ border: `1px solid ${C.border}`, borderRadius: 6, padding: "5px 8px", fontSize: 12, fontFamily: "inherit", color: C.text, background: C.white, cursor: "pointer" }}
        >
          <option value="none">No grouping</option>
          <option value="type">Group by type</option>
          <option value="zone">Group by zone</option>
        </select>

        {/* Flag filter */}
        {flagged.size > 0 && (
          <button
            onClick={() => setShowFlaggedOnly((v) => !v)}
            style={{
              padding: "5px 11px", borderRadius: 6, fontSize: 12, fontFamily: "inherit",
              border: `1.5px solid ${showFlaggedOnly ? "#f59e0b" : C.border}`,
              background: showFlaggedOnly ? "#fef3c7" : C.white,
              color: showFlaggedOnly ? "#92400e" : C.text,
              cursor: "pointer", fontWeight: showFlaggedOnly ? 700 : 400,
            }}
          >
            ★ Flagged ({flagged.size})
          </button>
        )}

        {/* Compact / Expanded */}
        <button
          onClick={() => setCompact((v) => !v)}
          style={{
            padding: "5px 11px", borderRadius: 6, fontSize: 12, fontFamily: "inherit",
            border: `1px solid ${C.border}`, background: C.white, color: C.text,
            cursor: "pointer",
          }}
        >
          {compact ? "⊞ Expanded" : "≡ Compact"}
        </button>

        {/* Column visibility (expanded mode only) */}
        {!compact && (
          <div style={{ position: "relative" }}>
            <button
              onClick={() => setColMenuOpen((v) => !v)}
              style={{ padding: "5px 11px", borderRadius: 6, fontSize: 12, fontFamily: "inherit", border: `1px solid ${C.border}`, background: C.white, color: C.text, cursor: "pointer" }}
            >
              Columns ▾
            </button>
            <Dropdown open={colMenuOpen} onClose={() => setColMenuOpen(false)} style={{ top: "calc(100% + 4px)", left: 0 }}>
              {COL_DEFS.map(({ key, label }) => (
                <label key={key} style={{ display: "flex", alignItems: "center", gap: 8, padding: "7px 14px", cursor: "pointer", fontSize: 13, color: C.text }}>
                  <input type="checkbox" checked={!!visibleCols[key]} onChange={() => toggleCol(key)} />
                  {label}
                </label>
              ))}
            </Dropdown>
          </div>
        )}

        {/* CSV export */}
        <button
          onClick={() => downloadCSV(sorted, compact ? { type: true, description: true, qty: true } : visibleCols)}
          style={{
            marginLeft: "auto", padding: "5px 13px", borderRadius: 6, fontSize: 12,
            fontFamily: "inherit", border: `1px solid ${C.navy}`, background: C.navy,
            color: C.white, cursor: "pointer", fontWeight: 600,
          }}
        >
          ↓ Export CSV
        </button>

        {/* Row count */}
        <span style={{ fontSize: 11, color: C.textMuted, whiteSpace: "nowrap" }}>
          Showing <strong>{filtered.length}</strong> of <strong>{allBom.length}</strong> items
        </span>
      </div>

      {/* ── TABLE ── */}
      <div style={{ overflowX: "auto" }}>
        <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 13 }}>
          <thead>
            <tr style={{ background: C.navy, color: C.white }}>
              {/* Flag col */}
              <th style={{ ...thStyle("center"), width: 36, padding: "9px 8px" }} />

              {activeCols.map(({ key, label, filterable }) => (
                <th
                  key={key}
                  onClick={() => handleSort(key === "description" ? "name" : key)}
                  style={{ ...thStyle(key === "qty" ? "right" : "left"), cursor: "pointer" }}
                >
                  <div style={{ display: "flex", alignItems: "center", gap: 5, justifyContent: key === "qty" ? "flex-end" : "flex-start" }}>
                    {filterable ? (
                      <div style={{ position: "relative" }}>
                        <button
                          onClick={(e) => { e.stopPropagation(); setTypeMenuOpen((v) => !v); }}
                          style={{ background: "none", border: "none", color: C.white, cursor: "pointer", fontSize: 11, padding: 0, fontFamily: "inherit", fontWeight: 600, letterSpacing: "0.07em", textTransform: "uppercase", display: "flex", alignItems: "center", gap: 4 }}
                        >
                          {label}
                          <span style={{ fontSize: 9, opacity: typeFilters.size ? 1 : 0.6 }}>
                            {typeFilters.size ? `▼ (${typeFilters.size})` : "▼"}
                          </span>
                        </button>
                        <Dropdown open={typeMenuOpen} onClose={() => setTypeMenuOpen(false)} style={{ top: "calc(100% + 6px)", left: 0 }}>
                          <div style={{ padding: "6px 14px 4px", display: "flex", justifyContent: "space-between", alignItems: "center" }}>
                            <span style={{ fontSize: 10, fontWeight: 700, color: C.textMuted, textTransform: "uppercase", letterSpacing: "0.06em" }}>Filter by Type</span>
                            {typeFilters.size > 0 && (
                              <button onClick={() => setTypeFilters(new Set())}
                                style={{ fontSize: 10, color: C.blue, background: "none", border: "none", cursor: "pointer", fontFamily: "inherit" }}>
                                Clear
                              </button>
                            )}
                          </div>
                          {allTypes.map((type) => (
                            <label key={type} style={{ display: "flex", alignItems: "center", gap: 8, padding: "6px 14px", cursor: "pointer", fontSize: 13, color: C.text }}>
                              <input type="checkbox" checked={typeFilters.has(type)} onChange={() => toggleTypeFilter(type)} />
                              <span style={{ width: 8, height: 8, borderRadius: "50%", background: typeColor(type), flexShrink: 0 }} />
                              {type}
                            </label>
                          ))}
                        </Dropdown>
                      </div>
                    ) : (
                      label
                    )}
                    <SortIcon col={key === "description" ? "name" : key} />
                  </div>
                </th>
              ))}
            </tr>
          </thead>

          {grouped.map(({ key: groupKey, items }) => (
            <GroupSection
              key={groupKey ?? "__all__"}
              groupKey={groupKey}
              items={items}
              activeCols={activeCols}
              compact={compact}
              flagged={flagged}
              onFlag={toggleFlag}
              tdStyle={tdStyle}
              groupBy={groupBy}
            />
          ))}

          <tfoot>
            <tr style={{ background: "#dbeafe" }}>
              <td />
              <td colSpan={activeCols.length - 1}
                style={{ padding: "10px 14px", fontWeight: 700, color: C.navy, fontSize: 12, letterSpacing: "0.06em", textTransform: "uppercase" }}>
                Total Unique Blocks
              </td>
              <td style={{ padding: "10px 14px", textAlign: "right", fontWeight: 800, color: C.navy, fontSize: 15 }}>
                {filtered.length}
              </td>
            </tr>
          </tfoot>
        </table>
      </div>
    </div>
  );
};

// ─────────────────────────────────────────────────────────────────────────────
// GROUP SECTION  (collapsible group within the BOM table)
// ─────────────────────────────────────────────────────────────────────────────
const GroupSection = ({ groupKey, items, activeCols, compact, flagged, onFlag, tdStyle, groupBy }) => {
  const [open, setOpen] = useState(true);

  return (
    <tbody>
      {groupKey !== null && (
        <tr
          onClick={() => setOpen((v) => !v)}
          style={{ background: "#e0e7ff", cursor: "pointer" }}
        >
          <td colSpan={activeCols.length + 1}
            style={{ padding: compact ? "6px 14px" : "8px 18px", fontWeight: 700, fontSize: 12, color: C.navy, letterSpacing: "0.05em" }}>
            <span style={{ marginRight: 8 }}>{open ? "▾" : "▸"}</span>
            {groupBy === "zone" ? `Zone ${groupKey}` : groupKey}
            <span style={{ marginLeft: 8, fontWeight: 400, color: C.textMuted, fontSize: 11 }}>
              ({items.length} item{items.length !== 1 ? "s" : ""})
            </span>
          </td>
        </tr>
      )}
      {open && items.map((item, i) => {
        const isFlagged = flagged.has(item.name);
        const color     = typeColor(item.type);
        return (
          <tr
            key={item.name}
            style={{
              background: isFlagged ? "#fef9c3" : i % 2 === 0 ? C.white : C.rowAlt,
              transition: "background 0.1s",
            }}
          >
            {/* Flag button */}
            <td style={{ ...tdStyle("center"), width: 36, padding: "6px 4px" }}>
              <button
                onClick={() => onFlag(item.name)}
                title={isFlagged ? "Remove flag" : "Flag row"}
                style={{
                  background: "none", border: "none", cursor: "pointer",
                  fontSize: 14, color: isFlagged ? "#f59e0b" : C.border,
                  lineHeight: 1, padding: 2,
                }}
              >
                ★
              </button>
            </td>

            {activeCols.map(({ key }) => {
              if (key === "type") return (
                <td key="type" style={{ ...tdStyle(), display: "table-cell" }}>
                  <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
                    <span style={{ width: 9, height: 9, borderRadius: "50%", background: color, flexShrink: 0 }} />
                    <span style={{ color: C.text }}>{item.type}</span>
                  </div>
                </td>
              );
              if (key === "description") return (
                <td key="description" style={{ ...tdStyle(), fontFamily: "monospace", fontSize: compact ? 11 : 12, color: C.text }}>
                  {item.name}
                </td>
              );
              if (key === "qty") return (
                <td key="qty" style={{ ...tdStyle("right"), color: C.navy, fontWeight: 700 }}>
                  {item.qty}
                </td>
              );
              if (key === "zone") return (
                <td key="zone" style={{ ...tdStyle("center"), color: C.textMuted, fontSize: 12 }}>
                  {zoneOf(item.eqNums)}
                </td>
              );
              if (key === "eqNums") return (
                <td key="eqNums" style={{ ...tdStyle(), fontFamily: "monospace", fontSize: 11, color: C.textMuted }}>
                  {item.eqNums.join(", ") || "—"}
                </td>
              );
              return null;
            })}
          </tr>
        );
      })}
    </tbody>
  );
};

// ─────────────────────────────────────────────────────────────────────────────
// MAIN APP
// ─────────────────────────────────────────────────────────────────────────────
export default function App() {
  const [rows, setRows]       = useState(null);
  const [tab, setTab]         = useState("bom");
  const [dragging, setDragging] = useState(false);
  const [fileName, setFileName] = useState("");

  const handleFile = (file) => {
    if (!file) return;
    setFileName(file.name);
    const reader = new FileReader();
    reader.onload = (e) => setRows(parseATTEXT(e.target.result));
    reader.readAsText(file);
  };

  const onDrop = useCallback((e) => {
    e.preventDefault();
    setDragging(false);
    handleFile(e.dataTransfer.files[0]);
  }, []);
  const onDragOver  = (e) => { e.preventDefault(); setDragging(true); };
  const onDragLeave = () => setDragging(false);

  return (
    <div style={{ minHeight: "100vh", background: C.offWhite, fontFamily: "'Barlow', 'Inter', 'Segoe UI', sans-serif", color: C.text }}>
      <style>{`
        @import url('https://fonts.googleapis.com/css2?family=Barlow:wght@400;500;600;700;800&display=swap');
        *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
        body { background: ${C.offWhite}; }
      `}</style>

      {/* ── HEADER ── */}
      <header style={{ background: C.white, borderBottom: `3px solid ${C.navy}`, padding: "0 40px", position: "sticky", top: 0, zIndex: 100, boxShadow: "0 1px 6px rgba(0,0,0,0.07)" }}>
        <div style={{ maxWidth: 1100, margin: "0 auto", display: "flex", alignItems: "center", height: 72 }}>
          <MTLogo size={1} />
          <div style={{ marginLeft: "auto", fontSize: 11, fontWeight: 700, color: C.textMuted, letterSpacing: "0.1em", textTransform: "uppercase", borderLeft: `2px solid ${C.border}`, paddingLeft: 18 }}>
            Equipment List Portal
          </div>
        </div>
      </header>

      {/* ── BANNER ── */}
      <div style={{ background: C.navy, color: C.white, padding: "30px 40px" }}>
        <div style={{ maxWidth: 1100, margin: "0 auto" }}>
          <h1 style={{ fontSize: 24, fontWeight: 800, letterSpacing: "0.07em", textTransform: "uppercase", marginBottom: 6 }}>
            Equipment List Portal
          </h1>
          <p style={{ fontSize: 13, opacity: 0.7, letterSpacing: "0.02em" }}>
            Upload an ATTEXT export (.txt) to generate a Bill of Materials and System Topology
          </p>
        </div>
      </div>

      {/* ── MAIN ── */}
      <main style={{ maxWidth: 1100, margin: "0 auto", padding: "36px 40px 60px" }}>
        {/* Upload zone */}
        <div
          onDrop={onDrop} onDragOver={onDragOver} onDragLeave={onDragLeave}
          onClick={() => document.getElementById("mt_file_input").click()}
          style={{
            border: `2px dashed ${dragging ? C.purple : C.border}`, borderRadius: 10, padding: "44px 32px",
            textAlign: "center", marginBottom: 36, background: dragging ? "#f5f3ff" : C.white,
            cursor: "pointer", transition: "all 0.15s ease", boxShadow: "0 1px 4px rgba(0,0,0,0.05)",
          }}
        >
          <input id="mt_file_input" type="file" accept=".txt,.csv" style={{ display: "none" }} onChange={(e) => handleFile(e.target.files[0])} />
          <svg width="44" height="44" viewBox="0 0 24 24" fill="none"
            stroke={dragging ? C.purple : C.textLight} strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"
            style={{ marginBottom: 12, display: "block", margin: "0 auto 12px" }}>
            <path d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1" />
            <polyline points="16 6 12 2 8 6" />
            <line x1="12" y1="2" x2="12" y2="15" />
          </svg>
          {fileName ? (
            <p style={{ color: C.green, fontWeight: 700, fontSize: 14, marginBottom: 4 }}>✓ &nbsp;{fileName}</p>
          ) : (
            <p style={{ color: C.textMuted, fontSize: 14, marginBottom: 4 }}>
              Drag &amp; drop your ATTEXT export here, or&nbsp;
              <span style={{ color: C.navy, fontWeight: 700 }}>click to browse</span>
            </p>
          )}
          {rows ? (
            <p style={{ fontSize: 12, color: C.textLight, marginTop: 6 }}>{rows.length} rows parsed</p>
          ) : (
            <p style={{ fontSize: 11, color: C.textLight, marginTop: 6 }}>Accepted formats: .txt &nbsp;·&nbsp; .csv</p>
          )}
        </div>

        {/* Results */}
        {rows && (
          <>
            {/* Tab bar */}
            <div style={{ display: "flex", borderBottom: `2px solid ${C.border}`, marginBottom: 24 }}>
              {[["bom", "Bill of Materials"], ["topology", "System Topology"]].map(([key, label]) => (
                <button key={key} onClick={() => setTab(key)}
                  style={{
                    padding: "10px 28px", fontSize: 12, fontWeight: 700, letterSpacing: "0.07em",
                    textTransform: "uppercase", border: "none", background: "none", cursor: "pointer",
                    color: tab === key ? C.navy : C.textMuted,
                    borderBottom: `3px solid ${tab === key ? C.navy : "transparent"}`,
                    marginBottom: -2, transition: "all 0.15s ease", fontFamily: "inherit",
                  }}>
                  {label}
                </button>
              ))}
            </div>

            {/* Content panel */}
            <div style={{ background: C.white, borderRadius: 8, overflow: "hidden", boxShadow: "0 1px 4px rgba(0,0,0,0.07)", border: `1px solid ${C.border}` }}>
              {tab === "bom"      && <BOMView rows={rows} />}
              {tab === "topology" && <div style={{ padding: 20 }}><TopologyView rows={rows} /></div>}
            </div>
          </>
        )}
      </main>

      {/* ── FOOTER ── */}
      <footer style={{ background: C.navyDark, color: C.white, padding: "22px 40px" }}>
        <div style={{ maxWidth: 1100, margin: "0 auto", display: "flex", justifyContent: "space-between", alignItems: "center" }}>
          <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
            <span style={{ fontSize: 16, fontWeight: 700, color: C.white }}>MotionTech</span>
            <svg width="20" height="20" viewBox="0 0 36 36" fill="none">
              <defs>
                <linearGradient id="mt_ring_footer" x1="4" y1="4" x2="32" y2="32" gradientUnits="userSpaceOnUse">
                  <stop offset="0%" stopColor="#a78bfa" />
                  <stop offset="100%" stopColor="#60a5fa" />
                </linearGradient>
              </defs>
              <circle cx="18" cy="18" r="13" stroke="url(#mt_ring_footer)" strokeWidth="4" fill="none" strokeLinecap="round" />
            </svg>
          </div>
          <span style={{ fontSize: 10, opacity: 0.45, letterSpacing: "0.1em", textTransform: "uppercase" }}>
            Equipment List Portal &nbsp;·&nbsp; Internal Tool
          </span>
        </div>
      </footer>
    </div>
  );
}
