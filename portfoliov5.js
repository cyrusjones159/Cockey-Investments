// ---- CONFIG -------------------------------------------------------------

const TOTAL_PORTFOLIO_TARGET = 50000;

const EPOCH_GROWTH = {
  epoch1: 1.10,
  epoch2: 1.20,
  epoch3: 1.30
};

// Example: you’ll replace these with your real arrays per sector
// Each item should match your JSON shape (ticker, totalspend, totalreturn, selected, priceEnd, etc.)
const SECTOR_DATA = {
  AUTOS: [ /* ... autos stocks ... */ ],
  FINANCIALS: [ /* ... financials stocks ... */ ],
  MEDIA: [ /* ... media stocks ... */ ],
  HEALTH: [ /* ... health stocks ... */ ],
  REITS: [ /* ... reits stocks ... */ ]
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

function normalizeSelected(val) {
  return String(val).toLowerCase() === "true";
}

function computeSectorAllocation(sectorName, tickers, growthFactors) {
  // EPOCH 1: all stocks
  const stocksEpoch1 = tickers;

  // EPOCH 2: selected stocks
  const stocksEpoch2 = tickers.filter(t => normalizeSelected(t.selected));

  // EPOCH 3: top 10 selected by totalreturn
  const stocksEpoch3 = [...stocksEpoch2]
    .sort((a, b) => b.totalreturn - a.totalreturn)
    .slice(0, 10);

  const epochCounts = {
    epoch1: stocksEpoch1.length,
    epoch2: stocksEpoch2.length,
    epoch3: stocksEpoch3.length
  };

  const sectorLabel = `${sectorName.toUpperCase()} (${stocksEpoch1.length})`;

  function computeActual(list) {
    return list.reduce((sum, t) => sum + (t.totalspend ?? 0), 0);
  }

  const actual1 = computeActual(stocksEpoch1);
  const actual2 = computeActual(stocksEpoch2);
  const actual3 = computeActual(stocksEpoch3);

  const projected = {
    epoch1: actual1 * growthFactors.epoch1,
    epoch2: actual2 * growthFactors.epoch2,
    epoch3: actual3 * growthFactors.epoch3
  };

  // Attach shares/price for modal (if not already present)
  function enrich(list) {
    return list.map(t => ({
      ...t,
      ticker: t.ticker,
      price: t.priceEnd ?? t.price ?? 0,
      shares: t.shares ?? (t.totalspend && (t.priceEnd || t.price)
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

  const autos = computeSectorAllocation("AUTOS", SECTOR_DATA.AUTOS, growth);
  const financials = computeSectorAllocation("FINANCIALS", SECTOR_DATA.FINANCIALS, growth);
  const media = computeSectorAllocation("MEDIA", SECTOR_DATA.MEDIA, growth);
  const health = computeSectorAllocation("HEALTH", SECTOR_DATA.HEALTH, growth);
  const reits = computeSectorAllocation("REITS", SECTOR_DATA.REITS, growth);

  const baseSectors = [autos, financials, media, health, reits];

  // CASH true-up per epoch so each epoch totals exactly 50k
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
      actual: cashActual.epoch1,
      totalspend: cashActual.epoch1
    }],
    epoch2: [{
      ticker: "CASH",
      price: 1,
      shares: cashActual.epoch2,
      actual: cashActual.epoch2,
      totalspend: cashActual.epoch2
    }],
    epoch3: [{
      ticker: "CASH",
      price: 1,
      shares: cashActual.epoch3,
      actual: cashActual.epoch3,
      totalspend: cashActual.epoch3
    }]
  };

  const cash = {
    sector: "CASH (1)",
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
