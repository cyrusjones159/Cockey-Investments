// ---- CONFIG -------------------------------------------------------------

const TOTAL_PORTFOLIO_TARGET = 50000;
const FIXED_SECTOR_TARGET = 10000; // for Autos, Financials, Media, Health

// Growth factors per epoch (example values)
const EPOCH_GROWTH = {
  epoch1: 1.10,
  epoch2: 1.20,
  epoch3: 1.30
};

// Core sector + ticker data
// Prices are example values; growth is applied via EPOCH_GROWTH
const SECTOR_DATA = {
  AUTOS: [
    { ticker: "TSLA", price: 180 },
    { ticker: "GM", price: 40 },
    { ticker: "F", price: 12 },
    { ticker: "TM", price: 190 },
    { ticker: "HMC", price: 32 }
  ],
  FINANCIALS: [
    { ticker: "BAC", price: 51 },
    { ticker: "JPM", price: 200 },
    { ticker: "WFC", price: 48 },
    { ticker: "C", price: 60 },
    { ticker: "MS", price: 90 }
  ],
  MEDIA: [
    { ticker: "DIS", price: 100 },
    { ticker: "NFLX", price: 450 },
    { ticker: "PARA", price: 15 },
    { ticker: "WBD", price: 10 },
    { ticker: "CMCSA", price: 40 }
  ],
  HEALTH: [
    { ticker: "PFE", price: 30 },
    { ticker: "JNJ", price: 160 },
    { ticker: "MRK", price: 110 },
    { ticker: "UNH", price: 500 },
    { ticker: "ABBV", price: 170 }
  ],
  // REITS is a single ETF-like instrument
  REITS: [
    { ticker: "VNQ", price: 80 }
  ]
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

// ---- CORE CALCULATION ---------------------------------------------------

function computeSectorAllocation(sectorName, tickers, growthFactors) {
  const isReits = sectorName === "REITS";
  const isCash = sectorName === "CASH";

  if (isCash) {
    // CASH is filled later in true-up; this function won't be used for CASH
    return null;
  }

  const count = tickers.length;
  const countLabel = isReits ? "REIT" : `${count} stocks`;

  let target = isReits ? 0 : FIXED_SECTOR_TARGET;
  let perStockTarget = isReits ? 0 : target / count;

  let stocks = [];
  let actual = 0;

  tickers.forEach(t => {
    let shares;
    if (isReits) {
      // REITS handled in true-up, not here
      shares = 0;
    } else {
      shares = Math.ceil(perStockTarget / t.price);
    }
    const cost = shares * t.price;
    actual += cost;
    stocks.push({
      ticker: t.ticker,
      price: t.price,
      shares,
      actual: cost
    });
  });

  const projected = {
    epoch1: actual * growthFactors.epoch1,
    epoch2: actual * growthFactors.epoch2,
    epoch3: actual * growthFactors.epoch3
  };

  return {
    sector: sectorName,
    countLabel,
    target,
    actual,
    projected,
    stocks
  };
}

function computeAllSectors() {
  // 1) Compute the four fixed sectors first
  const growth = EPOCH_GROWTH;

  const autos = computeSectorAllocation("AUTOS", SECTOR_DATA.AUTOS, growth);
  const financials = computeSectorAllocation(
    "FINANCIALS",
    SECTOR_DATA.FINANCIALS,
    growth
  );
  const media = computeSectorAllocation("MEDIA", SECTOR_DATA.MEDIA, growth);
  const health = computeSectorAllocation("HEALTH", SECTOR_DATA.HEALTH, growth);

  const fixedSectors = [autos, financials, media, health];

  const totalActualFixed = fixedSectors.reduce(
    (sum, s) => sum + s.actual,
    0
  );

  // 2) Remaining balance for REITS + CASH
  let remaining = TOTAL_PORTFOLIO_TARGET - totalActualFixed;
  if (remaining < 0) remaining = 0;

  // 3) REITS true-up (single ticker)
  const reitsTicker = SECTOR_DATA.REITS[0];
  const reitsPrice = reitsTicker.price;

  const reitsShares = Math.floor(remaining / reitsPrice);
  const reitsActual = reitsShares * reitsPrice;
  const reitsProjected = {
    epoch1: reitsActual * growth.epoch1,
    epoch2: reitsActual * growth.epoch2,
    epoch3: reitsActual * growth.epoch3
  };

  const reits = {
    sector: "REITS",
    countLabel: reitsShares > 0 ? `${reitsShares} shares` : "0 shares",
    target: remaining,
    actual: reitsActual,
    projected: reitsProjected,
    stocks: [
      {
        ticker: reitsTicker.ticker,
        price: reitsPrice,
        shares: reitsShares,
        actual: reitsActual
      }
    ]
  };

  // 4) CASH gets the leftover
  const cashActual = remaining - reitsActual;
  const cashProjected = {
    epoch1: cashActual,
    epoch2: cashActual,
    epoch3: cashActual
  };

  const cash = {
    sector: "CASH (TrueUp to 50K)",
    countLabel: "1 share",
    target: remaining,
    actual: cashActual,
    projected: cashProjected,
    stocks: [
      {
        ticker: "CASH",
        price: 1,
        shares: 1,
        actual: cashActual
      }
    ]
  };

  return [...fixedSectors, reits, cash];
}

function computeTotals(sectors) {
  const totals = {
    epoch1: { target: 0, actual: 0, projected: 0 },
    epoch2: { target: 0, actual: 0, projected: 0 },
    epoch3: { target: 0, actual: 0, projected: 0 }
  };

  sectors.forEach(s => {
    totals.epoch1.target += s.target;
    totals.epoch1.actual += s.actual;
    totals.epoch1.projected += s.projected.epoch1;

    totals.epoch2.target += s.target;
    totals.epoch2.actual += s.actual;
    totals.epoch2.projected += s.projected.epoch2;

    totals.epoch3.target += s.target;
    totals.epoch3.actual += s.actual;
    totals.epoch3.projected += s.projected.epoch3;
  });

  return totals;
}

// ---- RENDERING -----------------------------------------------------------

function buildTable() {
  const tbody = document.getElementById("portfolioBody");
  const trowTotals = document.getElementById("totalsRow");
  tbody.innerHTML = "";
  trowTotals.innerHTML = "";

  const sectors = computeAllSectors();
  const totals = computeTotals(sectors);

  sectors.forEach(s => {
    const tr = document.createElement("tr");

    // Sector name
    const tdSector = document.createElement("td");
    tdSector.textContent = s.sector;
    tr.appendChild(tdSector);

    // Helper to build epoch cell
    function buildEpochCell(epochKey, epochLabel) {
      const td = document.createElement("td");
      td.className = "epoch-cell";
      td.innerHTML = makeEpochQuad(
        s.countLabel,
        s.target,
        s.actual,
        s.projected[epochKey]
      );
      td.dataset.sector = s.sector;
      td.dataset.epoch = epochLabel;
      td.addEventListener("click", onEpochCellClick);
      return td;
    }

    // Epoch1
    tr.appendChild(buildEpochCell("epoch1", "Epoch 1"));
    // Epoch2
    tr.appendChild(buildEpochCell("epoch2", "Epoch 2"));
    // Epoch3
    tr.appendChild(buildEpochCell("epoch3", "Epoch 3"));

    // Ratios (based on projected)
    const tdRatios = document.createElement("td");
    tdRatios.className = "ratios-cell";

    if (s.sector.startsWith("CASH")) {
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

// ---- MODAL HANDLING ------------------------------------------------------

function onEpochCellClick(e) {
  const sectorName = e.currentTarget.dataset.sector;
  const epochLabel = e.currentTarget.dataset.epoch;

  const sectors = computeAllSectors();
  const sector = sectors.find(s => s.sector === sectorName);
  if (!sector) return;

  const title = document.getElementById("breakoutTitle");
  const body = document.getElementById("breakoutBody");
  title.textContent = `${sectorName} — ${epochLabel} Share Breakout`;
  body.innerHTML = "";

  sector.stocks.forEach(st => {
    const tr = document.createElement("tr");

    const tdTicker = document.createElement("td");
    tdTicker.textContent = st.ticker;
    tr.appendChild(tdTicker);

    const tdShares = document.createElement("td");
    tdShares.textContent = st.shares;
    tr.appendChild(tdShares);

    const tdPrice = document.createElement("td");
    tdPrice.textContent = formatNumber(st.price);
    tr.appendChild(tdPrice);

    const tdActual = document.createElement("td");
    tdActual.textContent = formatNumber(st.actual);
    tr.appendChild(tdActual);

    body.appendChild(tr);
  });

  const modalEl = document.getElementById("breakoutModal");
  const modal = bootstrap.Modal.getOrCreateInstance(modalEl);
  modal.show();
}

// ---- INIT ----------------------------------------------------------------

document.getElementById("loadBtn").addEventListener("click", () => {
  buildTable();
});

// Auto-load on page open
buildTable();
