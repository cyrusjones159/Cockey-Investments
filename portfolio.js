// ---- CONFIG -------------------------------------------------------------
//----JSS VERSION 5.1 - 

const TOTAL_PORTFOLIO_TARGET = 50000;
const FIXED_SECTOR_TARGET = 10000;

// IMPORTANT:
// Replace these arrays with your real JSON-loaded arrays.
// Each item must match your JSON structure:
// {
//   ticker,
//   sector,
//   priceStart,
//   priceEnd,
//   totalspend,
//   totalreturn,
//   totalfiveyearview,
//   selected
// }
const SECTOR_DATA = {
  AUTOS: [],
  FINANCIALS: [],
  MEDIA: [],
  HEALTH: [],
  REITS: []
};

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
  return String(val).toLowerCase() === "true";
}

function safePrice(t) {
  const p = t.priceEnd ?? t.price ?? 0;
  return p > 0 ? p : 0;
}

function safeFiveYear(t) {
  return t.totalfiveyearview ?? 0;
}

// ---- CORE PER-EPOCH BUILDERS -------------------------------------------

// For Autos, Financials, Health, Media (fixed 10k target, CEILING)
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

// For REITS (balance sector, FLOOR)
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

// Fixed 10k sectors: Autos, Financials, Health, Media
function computeFixedSector(displayName, tickers) {
  // Epoch 1: all
  const epoch1Tickers = tickers;

  // Epoch 2: selected
  const epoch2Tickers = tickers.filter(t => normalizeSelected(t.selected));

  // Epoch 3: top 10 selected by totalreturn
  const epoch3Tickers = [...epoch2Tickers]
    .sort((a, b) => b.totalreturn - a.totalreturn)
    .slice(0, 10);

  const e1 = buildEpochStocksFixedTarget(epoch1Tickers, FIXED_SECTOR_TARGET);
  const e2 = buildEpochStocksFixedTarget(epoch2Tickers, FIXED_SECTOR_TARGET);
  const e3 = buildEpochStocksFixedTarget(epoch3Tickers, FIXED_SECTOR_TARGET);

  const sectorLabel = `${displayName} (${epoch1Tickers.length})`;

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

  // Epoch 3: top 10 selected by totalreturn
  const epoch3Tickers = [...epoch2Tickers]
    .sort((a, b) => b.totalreturn - a.totalreturn)
    .slice(0, 10);

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

  return {
    sector: sectorLabel,
    // Display target is always 10k, even though true target is smaller
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

  return {
    sector: "Cash (1)",
    target: cashActual.epoch1, // display as its own amount
    epochCounts: { epoch1: 1, epoch2: 1, epoch3: 1 },
    actual: cashActual,
    projected: cashProjected,
    stocks: cashStocks
  };
}

// ---- PUBLIC API USED BY RENDERING --------------------------------------

function computeAllSectors() {
  // 1–4: fixed 10k sectors
  const autos      = computeFixedSector("Autos",      SECTOR_DATA.AUTOS);
  const financials = computeFixedSector("Financials", SECTOR_DATA.FINANCIALS);
  const health     = computeFixedSector("Health",     SECTOR_DATA.HEALTH);
  const media      = computeFixedSector("Media",      SECTOR_DATA.MEDIA);

  const baseSectors = [autos, financials, health, media];

  // 5: REITS as balance
  const reits = computeReitsSector("REITs", SECTOR_DATA.REITS, baseSectors);

  const basePlusReits = [...baseSectors, reits];

  // 6: CASH as leftover
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

    // Sector label already includes total stock count, e.g. "Financials (22)"
    const tdSector = document.createElement("td");
    tdSector.textContent = s.sector;
    tr.appendChild(tdSector);

    function buildEpochCell(epochKey) {
      const td = document.createElement("td");
      td.className = "epoch-cell";

      td.innerHTML = makeEpochQuad(
        s.epochCounts[epochKey],   // epoch-specific count
        s.target,                  // 10k for first 4, 10k for REITs, cash amount for Cash
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

    // Ratios
    const tdRatios = document.createElement("td");
    tdRatios.className = "ratios-cell";

    if (s.sector.toUpperCase().startsWith("CASH")) {
      tdRatios.textContent = "N/A";
    } else {
      const rE2E1 = ratio(s.projected.epoch2, s.projected.epoch1);
      const rE3E1 = ratio(s.projected.epoch3, s.projected.epoch1);
      tdRatios.innerHTML = `E2/E1: ${rE2E1}&nbsp;&nbsp;E3/E1: ${rE3E1}`;
    }

    tr.appendChild(tdRatios);
    tbody.appendChild(tr);
  });

  // Totals row
  const tdLabel = document.createElement("td");
  tdLabel.textContent = "TOTALS";
  trowTotals.appendChild(tdLabel);

  function buildTotalsEpochCell(epochKey) {
    const td = document.createElement("td");
    td.className = "epoch-cell";
    const t = totals[epochKey];

    td.innerHTML = makeEpochQuad(
      "Totals",
      t.target,       // always 50k at totals level
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

// ---- INIT ---------------------------------------------------------------

document.addEventListener("DOMContentLoaded", () => {
  const loadBtn = document.getElementById("loadBtn");
  if (loadBtn) {
    loadBtn.addEventListener("click", buildTable);
  }
  // Initial render
  buildTable();
});
