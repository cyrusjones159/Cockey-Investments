// ---- CONFIG -------------------------------------------------------------

const TOTAL_PORTFOLIO_TARGET = 50000;

const EPOCH_GROWTH = {
  epoch1: 1.10,
  epoch2: 1.20,
  epoch3: 1.30
};

// IMPORTANT:
// Replace these arrays with your real JSON-loaded arrays.
// Each item must match your JSON structure:
// { ticker, sector, priceEnd, totalspend, totalreturn, selected }
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

// ---- CORE CALCULATION ---------------------------------------------------

function computeSectorAllocation(displayName, tickers, growthFactors) {
  // ---- EPOCH 1: all stocks ----
  const stocksEpoch1 = tickers;

  // ---- EPOCH 2: selected stocks ----
  const stocksEpoch2 = tickers.filter(t => normalizeSelected(t.selected));

  // ---- EPOCH 3: top 10 selected by totalreturn ----
  const stocksEpoch3 = [...stocksEpoch2]
    .sort((a, b) => b.totalreturn - a.totalreturn)
    .slice(0, 10);

  // ---- Epoch counts ----
  const epochCounts = {
    epoch1: stocksEpoch1.length,
    epoch2: stocksEpoch2.length,
    epoch3: stocksEpoch3.length
  };

  // ---- Sector label with total stocks ----
  const sectorLabel = `${displayName} (${stocksEpoch1.length})`;

  // ---- Compute actual cost for each epoch ----
  function computeActual(list) {
    return list.reduce((sum, t) => sum + (t.totalspend ?? 0), 0);
  }

  const actual1 = computeActual(stocksEpoch1);
  const actual2 = computeActual(stocksEpoch2);
  const actual3 = computeActual(stocksEpoch3);

  // ---- Projected values ----
  const projected = {
    epoch1: actual1 * growthFactors.epoch1,
    epoch2: actual2 * growthFactors.epoch2,
    epoch3: actual3 * growthFactors.epoch3
  };

  // ---- Enrich stocks for modal ----
  function enrich(list) {
    return list.map(t => ({
      ...t,
      ticker: t.ticker,
      price: t.priceEnd ?? t.price ?? 0,
      shares:
        t.shares ??
        (t.totalspend && (t.priceEnd || t.price)
          ? t.totalspend / (t.priceEnd || t.price)
          : undefined),
      actual: t.totalspend ?? 0
    }));
  }

  return {
    sector: sectorLabel,
    epochCounts,
    actual: {
      epoch1: actual1,
      epoch2: actual2,
      epoch3: actual3
    },
    projected,
    stocks: {
      epoch1: enrich(stocksEpoch1),
      epoch2: enrich(stocksEpoch2),
      epoch3: enrich(stocksEpoch3)
    }
  };
}

function computeAllSectors() {
  const growth = EPOCH_GROWTH;

  // Build each sector
  const autos       = computeSectorAllocation("Autos",       SECTOR_DATA.AUTOS,       growth);
  const financials  = computeSectorAllocation("Financials",  SECTOR_DATA.FINANCIALS,  growth);
  const media       = computeSectorAllocation("Media",       SECTOR_DATA.MEDIA,       growth);
  const health      = computeSectorAllocation("Health",      SECTOR_DATA.HEALTH,      growth);
  const reits       = computeSectorAllocation("REITs",       SECTOR_DATA.REITS,       growth);

  const baseSectors = [autos, financials, media, health, reits];

  // ---- CASH TRUE-UP PER EPOCH ----
  const cashActual = {
    epoch1: 0,
    epoch2: 0,
    epoch3: 0
  };

  ["epoch1", "epoch2", "epoch3"].forEach(epochKey => {
    const totalActual = baseSectors.reduce(
      (sum, s) => sum + s.actual[epochKey],
      0
    );
    cashActual[epochKey] = TOTAL_PORTFOLIO_TARGET - totalActual;
  });

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
      actual: cashActual.epoch1
    }],
    epoch2: [{
      ticker: "CASH",
      price: 1,
      shares: cashActual.epoch2,
      actual: cashActual.epoch2
    }],
    epoch3: [{
      ticker: "CASH",
      price: 1,
      shares: cashActual.epoch3,
      actual: cashActual.epoch3
    }]
  };

  const cash = {
    sector: "Cash (1)",
    epochCounts: { epoch1: 1, epoch2: 1, epoch3: 1 },
    actual: cashActual,
    projected: cashProjected,
    stocks: cashStocks
  };

  return [...baseSectors, cash];
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
