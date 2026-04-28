// ---- CONFIG -------------------------------------------------------------
//VERSION 5.3 JOHN S, STRITZINGER

const TOTAL_PORTFOLIO_TARGET = 50000;
const FIXED_SECTOR_TARGET = 10000;
const API_URL =
  "https://cockyinvestments-ghehe0a8a7cge7fk.westus3-01.azurewebsites.net/api/Sectorsummaries";

// Will be filled from API
let SECTOR_DATA = {
  autos: [],
  financials: [],
  health: [],
  media: [],
  reits: []
};

let GLOBAL_TOP10 = []; // top 10 by totalreturn across all stocks

// ---- HELPERS ------------------------------------------------------------

function formatNumber(n) {
  return n.toLocaleString("en-US", {
    minimumFractionDigits: 2,
    maximumFractionDigits: 2
  });
}

function formatInt(n) {
  return n.toLocaleString("en-US", {
    minimumFractionDigits: 0,
    maximumFractionDigits: 0
  });
}

function ratio(a, b) {
  if (!b || b === 0) return "—";
  return (a / b).toFixed(2);
}

function makeEpochQuad(label, target, actual, projected) {
  return `
    <div class="epoch-quad">
      <span>${label}</span>
      <span>${formatInt(target)}</span>
      <span>${formatNumber(actual)}</span>
      <span>${formatNumber(projected)}</span>
    </div>
  `;
}

function normalizeSelected(val) {
  if (val === true) return true;
  if (typeof val === "string") {
    const v = val.trim().toLowerCase();
    return v === "true" || v === "yes" || v === "y";
  }
  return false;
}

function safePrice(t) {
  const p = t.priceEnd ?? t.price ?? 0;
  return p > 0 ? p : 0;
}

function safeFiveYear(t) {
  return t.totalfiveyearview ?? 0;
}

function isInGlobalTop10(stock) {
  if (!GLOBAL_TOP10 || GLOBAL_TOP10.length === 0) return false;
  if (stock.id != null) {
    return GLOBAL_TOP10.some(s => s.id === stock.id);
  }
  return GLOBAL_TOP10.some(s => s.ticker === stock.ticker);
}

// ---- CORE PER-EPOCH BUILDERS -------------------------------------------

// Fixed 10k sectors: autos, financials, health, media
function buildEpochStocksFixedTarget(tickers, sectorTarget) {
  const N = tickers.length;
  if (N === 0) {
    return {
      stocks: [],
      count: 0,
      actual: 0,
      projected: 0
    };
  }

  const perStockTarget = sectorTarget / N;
  let actualSum = 0;
  let projectedSum = 0;

  const stocks = tickers.map(t => {
    const price = safePrice(t);
    let shares = 0;
    if (price > 0) {
      shares = Math.ceil(perStockTarget / price);
    }
    const actual = shares * price;
    const projected = shares * safeFiveYear(t);

    actualSum += actual;
    projectedSum += projected;

    return {
      ...t,
      price,
      shares,
      actual,
      projected
    };
  });

  return {
    stocks,
    count: N,
    actual: actualSum,
    projected: projectedSum
  };
}

// REITS: balance sector
function buildEpochStocksReits(tickers, reitsTarget) {
  const N = tickers.length;
  if (N === 0 || reitsTarget <= 0) {
    return {
      stocks: [],
      count: 0,
      actual: 0,
      projected: 0
    };
  }

  const perStockTarget = reitsTarget / N;
  let actualSum = 0;
  let projectedSum = 0;

  const stocks = tickers.map(t => {
    const price = safePrice(t);
    let shares = 0;
    if (price > 0) {
      shares = Math.floor(perStockTarget / price);
      if (shares < 0) shares = 0;
    }
    const actual = shares * price;
    const projected = shares * safeFiveYear(t);

    actualSum += actual;
    projectedSum += projected;

    return {
      ...t,
      price,
      shares,
      actual,
      projected
    };
  });

  return {
    stocks,
    count: N,
    actual: actualSum,
    projected: projectedSum
  };
}

// ---- SECTOR BUILDERS ----------------------------------------------------

// Fixed 10k sectors: autos, financials, health, media
function computeFixedSector(displayName, tickers) {
  // Epoch 1: all
  const epoch1Tickers = tickers;

  // Epoch 2: selected
  const epoch2Tickers = tickers.filter(t => normalizeSelected(t.selected));

  // Epoch 3: global top 10 by totalreturn (from all stocks), restricted to this sector
  const epoch3Tickers = epoch1Tickers.filter(t => isInGlobalTop10(t));

  const e1 = buildEpochStocksFixedTarget(epoch1Tickers, FIXED_SECTOR_TARGET);
  const e2 = buildEpochStocksFixedTarget(epoch2Tickers, FIXED_SECTOR_TARGET);
  const e3 = buildEpochStocksFixedTarget(epoch3Tickers, FIXED_SECTOR_TARGET);

  const sectorLabel = `${displayName} (${epoch1Tickers.length})`;

  // Hypothesis: Model 2 = actualEpoch1 * 1.2
  const hypothesis = e1.actual * 1.2;

  return {
    sector: sectorLabel,
    target: FIXED_SECTOR_TARGET,
    epochCounts: {
      epoch1: e1.count,
      epoch2: e2.count,
      epoch3: e3.count
    },
    actual: {
      epoch1: e1.actual,
      epoch2: e2.actual,
      epoch3: e3.actual
    },
    projected: {
      epoch1: e1.projected,
      epoch2: e2.projected,
      epoch3: e3.projected
    },
    hypothesis,
    stocks: {
      epoch1: e1.stocks,
      epoch2: e2.stocks,
      epoch3: e3.stocks
    }
  };
}

// REITS: balance sector
function computeReitsSector(displayName, tickers, baseSectors) {
  // Epoch 1: all
  const epoch1Tickers = tickers;

  // Epoch 2: selected
  const epoch2Tickers = tickers.filter(t => normalizeSelected(t.selected));

  // Epoch 3: global top 10 by totalreturn, restricted to this sector
  const epoch3Tickers = epoch1Tickers.filter(t => isInGlobalTop10(t));

  function totalActualOfBase(epochKey) {
    return baseSectors.reduce((sum, s) => sum + s.actual[epochKey], 0);
  }

  const reitsTargets = {
    epoch1: TOTAL_PORTFOLIO_TARGET - totalActualOfBase("epoch1"),
    epoch2: TOTAL_PORTFOLIO_TARGET - totalActualOfBase("epoch2"),
    epoch3: TOTAL_PORTFOLIO_TARGET - totalActualOfBase("epoch3")
  };

  const e1 = buildEpochStocksReits(epoch1Tickers, reitsTargets.epoch1);
  const e2 = buildEpochStocksReits(epoch2Tickers, reitsTargets.epoch2);
  const e3 = buildEpochStocksReits(epoch3Tickers, reitsTargets.epoch3);

  const sectorLabel = `${displayName} (${epoch1Tickers.length})`;

  const hypothesis = e1.actual * 1.2;

  return {
    sector: sectorLabel,
    target: FIXED_SECTOR_TARGET,
    epochCounts: {
      epoch1: e1.count,
      epoch2: e2.count,
      epoch3: e3.count
    },
    actual: {
      epoch1: e1.actual,
      epoch2: e2.actual,
      epoch3: e3.actual
    },
    projected: {
      epoch1: e1.projected,
      epoch2: e2.projected,
      epoch3: e3.projected
    },
    hypothesis,
    stocks: {
      epoch1: e1.stocks,
      epoch2: e2.stocks,
      epoch3: e3.stocks
    }
  };
}

// CASH: leftover
function computeCashSector(baseSectorsPlusReits) {
  function totalActual(epochKey) {
    return baseSectorsPlusReits.reduce(
      (sum, s) => sum + s.actual[epochKey],
      0
    );
  }

  const cashActual = {
    epoch1: TOTAL_PORTFOLIO_TARGET - totalActual("epoch1"),
    epoch2: TOTAL_PORTFOLIO_TARGET - totalActual("epoch2"),
    epoch3: TOTAL_PORTFOLIO_TARGET - totalActual("epoch3")
  };

  const cashProjected = {
    epoch1: cashActual.epoch1,
    epoch2: cashActual.epoch2,
    epoch3: cashActual.epoch3
  };

  const cashStocks = {
    epoch1: [{
      ticker: "CASH",
      price: 1,
      shares: cashActual.epoch1,
      actual: cashActual.epoch1,
      projected: cashProjected.epoch1
    }],
    epoch2: [{
      ticker: "CASH",
      price: 1,
      shares: cashActual.epoch2,
      actual: cashActual.epoch2,
      projected: cashProjected.epoch2
    }],
    epoch3: [{
      ticker: "CASH",
      price: 1,
      shares: cashActual.epoch3,
      actual: cashActual.epoch3,
      projected: cashProjected.epoch3
    }]
  };

  // For cash, hypothesis doesn't really apply; set to 0
  return {
    sector: "cash (1)",
    target: cashActual.epoch1,
    epochCounts: { epoch1: 1, epoch2: 1, epoch3: 1 },
    actual: cashActual,
    projected: cashProjected,
    hypothesis: 0,
    stocks: cashStocks
  };
}

// ---- PUBLIC API USED BY RENDERING --------------------------------------

function computeAllSectors() {
  const autos      = computeFixedSector("autos",      SECTOR_DATA.autos);
  const financials = computeFixedSector("financials", SECTOR_DATA.financials);
  const health     = computeFixedSector("health",     SECTOR_DATA.health);
  const media      = computeFixedSector("media",      SECTOR_DATA.media);

  const baseSectors = [autos, financials, health, media];

  const reits = computeReitsSector("reits", SECTOR_DATA.reits, baseSectors);

  const basePlusReits = [...baseSectors, reits];

  const cash = computeCashSector(basePlusReits);

  return [...basePlusReits, cash];
}

function computeTotals(sectors) {
  const totals = {
    epoch1: { target: TOTAL_PORTFOLIO_TARGET, actual: 0, projected: 0 },
    epoch2: { target: TOTAL_PORTFOLIO_TARGET, actual: 0, projected: 0 },
    epoch3: { target: TOTAL_PORTFOLIO_TARGET, actual: 0, projected: 0 }
  };

  sectors.forEach(s => {
    totals.epoch1.actual += s.actual.epoch1;
    totals.epoch1.projected += s.projected.epoch1;

    totals.epoch2.actual += s.actual.epoch2;
    totals.epoch2.projected += s.projected.epoch2;

    totals.epoch3.actual += s.actual.epoch3;
    totals.epoch3.projected += s.projected.epoch3;
  });

  return totals;
}

// ---- RENDERING / DOM WIRING --------------------------------------------

function buildTable() {
  const tbody = document.getElementById("portfolioBody");
  const trowTotals = document.getElementById("totalsRow");
  if (!tbody || !trowTotals) return;

  tbody.innerHTML = "";
  trowTotals.innerHTML = "";

  const sectors = computeAllSectors();
  const totals = computeTotals(sectors);

  sectors.forEach(s => {
    const tr = document.createElement("tr");

    const tdSector = document.createElement("td");
    tdSector.textContent = s.sector;
    tr.appendChild(tdSector);

    function buildEpochCell(epochKey) {
      const td = document.createElement("td");
      td.className = "epoch-cell";

      td.innerHTML = makeEpochQuad(
        s.epochCounts[epochKey],
        s.target,
        s.actual[epochKey],
        s.projected[epochKey]
      );

      const btn = document.createElement("button");
      btn.className = "btn btn-sm btn-outline-primary mt-1";
      btn.textContent = "View Stocks";
      btn.dataset.sector = s.sector;
      btn.dataset.epoch = epochKey;
      btn.addEventListener("click", onEpochButtonClick);

      td.appendChild(btn);
      return td;
    }

    tr.appendChild(buildEpochCell("epoch1"));
    tr.appendChild(buildEpochCell("epoch2"));
    tr.appendChild(buildEpochCell("epoch3"));

    const tdRatios = document.createElement("td");
    tdRatios.className = "ratios-cell";

    if (s.sector.toLowerCase().startsWith("cash")) {
      tdRatios.textContent = "N/A";
    } else {
      const rE2E1 = ratio(s.projected.epoch2, s.projected.epoch1);
      const rE3E1 = ratio(s.projected.epoch3, s.projected.epoch1);

      const hyp = s.hypothesis || 0;
      const rE2H = hyp ? ratio(s.projected.epoch2, hyp) : "—";
      const rE3H = hyp ? ratio(s.projected.epoch3, hyp) : "—";

      tdRatios.textContent =
        `E2/E1: ${rE2E1} | E3/E1: ${rE3E1} | E2/H: ${rE2H} | E3/H: ${rE3H}`;
    }

    tr.appendChild(tdRatios);
    tbody.appendChild(tr);
  });

  const tdLabel = document.createElement("td");
  tdLabel.textContent = "TOTALS";
  trowTotals.appendChild(tdLabel);

  function buildTotalsEpochCell(epochKey) {
    const td = document.createElement("td");
    td.className = "epoch-cell";
    const t = totals[epochKey];

    td.innerHTML = makeEpochQuad(
      "Totals",
      t.target,
      t.actual,
      t.projected
    );
    return td;
  }

  trowTotals.appendChild(buildTotalsEpochCell("epoch1"));
  trowTotals.appendChild(buildTotalsEpochCell("epoch2"));
  trowTotals.appendChild(buildTotalsEpochCell("epoch3"));

  const tdRatiosTotal = document.createElement("td");
  tdRatiosTotal.textContent = "—";
  trowTotals.appendChild(tdRatiosTotal);
}

function onEpochButtonClick(e) {
  const sectorName = e.currentTarget.dataset.sector;
  const epochKey = e.currentTarget.dataset.epoch;

  const sectors = computeAllSectors();
  const sector = sectors.find(s => s.sector === sectorName);
  if (!sector) return;

  const stockList = sector.stocks[epochKey];

  const title = document.getElementById("breakoutTitle");
  const body = document.getElementById("breakoutBody");
  if (!title || !body) return;

  title.textContent = `${sectorName} — ${epochKey.toUpperCase()} Stocks`;
  body.innerHTML = "";

  stockList.forEach(st => {
    const tr = document.createElement("tr");
    tr.innerHTML = `
      <td>${st.ticker}</td>
      <td>${st.shares ?? "-"}</td>
      <td>${formatNumber(st.price)}</td>
      <td>${formatNumber(st.actual)}</td>
      <td>${formatNumber(st.projected ?? 0)}</td>
    `;
    body.appendChild(tr);
  });

  const modalEl = document.getElementById("breakoutModal");
  if (!modalEl) return;
  const modal = bootstrap.Modal.getOrCreateInstance(modalEl);
  modal.show();
}

// ---- DATA LOADING -------------------------------------------------------

async function loadSectorData() {
  try {
    const response = await fetch(API_URL, {
      method: "GET",
      headers: {
        Accept: "application/json"
      }
    });

    if (!response.ok) {
      console.error("API error:", response.status, response.statusText);
      return;
    }

    const data = await response.json();
    if (!Array.isArray(data)) {
      console.error("Unexpected API shape, expected array");
      return;
    }

    // Group by sector (lowercase)
    SECTOR_DATA = {
      autos: data.filter(x => (x.sector || "").toLowerCase() === "autos"),
      financials: data.filter(x => (x.sector || "").toLowerCase() === "financials"),
      health: data.filter(x => (x.sector || "").toLowerCase() === "health"),
      media: data.filter(x => (x.sector || "").toLowerCase() === "media"),
      reits: data.filter(x => (x.sector || "").toLowerCase() === "reits")
    };

    // Compute global top 10 by totalreturn across all stocks
    const allStocks = data.slice();
    allStocks.sort((a, b) => (b.totalreturn || 0) - (a.totalreturn || 0));
    GLOBAL_TOP10 = allStocks.slice(0, 10);

    buildTable();
  } catch (err) {
    console.error("Error loading sector data:", err);
  }
}

// ---- INIT ---------------------------------------------------------------

document.addEventListener("DOMContentLoaded", () => {
  const loadBtn = document.getElementById("loadBtn");
  if (loadBtn) {
    loadBtn.addEventListener("click", () => {
      loadSectorData();
    });
  }

  // Initial load
  loadSectorData();
});
