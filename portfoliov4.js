//VERSION 4 OF THE PORFTFOLIO MODELER
// ---- CONFIG -------------------------------------------------------------

const TOTAL_PORTFOLIO_TARGET = 50000;
const FIXED_SECTOR_TARGET = 10000;

const EPOCH_GROWTH = {
  epoch1: 1.10,
  epoch2: 1.20,
  epoch3: 1.30
};

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

  const count = tickers.length;

  // NEW: epoch-specific counts
  const epochCounts = {
    epoch1: count,
    epoch2: count,
    epoch3: count
  };

  const countLabel = `${count}`;

  let target = FIXED_SECTOR_TARGET;
  let perStockTarget = target / count;

  let stocks = [];
  let actual = 0;

  tickers.forEach(t => {
    const shares = Math.ceil(perStockTarget / t.price);
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
    sector: `${sectorName} (${count})`,   // NEW: sector label includes count
    epochCounts,
    target,
    actual,
    projected,
    stocks
  };
}

function computeAllSectors() {
  const growth = EPOCH_GROWTH;

  // Fixed sectors
  const autos = computeSectorAllocation("AUTOS", SECTOR_DATA.AUTOS, growth);
  const financials = computeSectorAllocation("FINANCIALS", SECTOR_DATA.FINANCIALS, growth);
  const media = computeSectorAllocation("MEDIA", SECTOR_DATA.MEDIA, growth);
  const health = computeSectorAllocation("HEALTH", SECTOR_DATA.HEALTH, growth);

  const fixed = [autos, financials, media, health];

  // Compute REITS first
  const reitsTicker = SECTOR_DATA.REITS[0];
  const reitsPrice = reitsTicker.price;

  // REITS gets whatever shares fit into remaining after fixed sectors
  const fixedActual = fixed.reduce((sum, s) => sum + s.actual, 0);
  const remainingAfterFixed = TOTAL_PORTFOLIO_TARGET - fixedActual;

  const reitsShares = Math.floor(remainingAfterFixed / reitsPrice);
  const reitsActual = reitsShares * reitsPrice;

  const reitsProjected = {
    epoch1: reitsActual * growth.epoch1,
    epoch2: reitsActual * growth.epoch2,
    epoch3: reitsActual * growth.epoch3
  };

  const reits = {
    sector: `REITS (1)`,
    epochCounts: { epoch1: 1, epoch2: 1, epoch3: 1 },
    target: remainingAfterFixed,
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

  // NEW: CASH computed AFTER REITS
  const cashActual = TOTAL_PORTFOLIO_TARGET -
    (fixedActual + reitsActual);

  const cash = {
    sector: "CASH (1)",
    epochCounts: { epoch1: 1, epoch2: 1, epoch3: 1 },
    target: cashActual,
    actual: cashActual,
    projected: {
      epoch1: cashActual,
      epoch2: cashActual,
      epoch3: cashActual
    },
    stocks: [
      { ticker: "CASH", price: 1, shares: 1, actual: cashActual }
    ]
  };

  return [...fixed, reits, cash];
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
