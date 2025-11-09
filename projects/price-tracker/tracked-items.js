// tracked-items.js - Manage all tracked items page

class TrackedItemsManager {
  constructor() {
    this.items = [];
    this.filteredItems = [];

    this.init();
  }

  async init() {
    await this.loadItems();
    this.setupEventListeners();
    this.renderItems();
    this.updateStats();
    document.getElementById("loading").style.display = "none";
  }

  setupEventListeners() {
    document.getElementById("search").addEventListener("input", (e) => {
      this.filterItems(e.target.value);
    });

    document.getElementById("check-prices").addEventListener("click", () => {
      this.checkAllPrices();
    });

    document.getElementById("export-data").addEventListener("click", () => {
      this.exportData();
    });

    document.getElementById("clear-all").addEventListener("click", () => {
      this.clearAllItems();
    });
  }

  async loadItems() {
    const result = await browser.storage.local.get(["trackedItems"]);
    this.items = result.trackedItems || [];

    // Ensure backward compatibility by adding priceHistory to items that don't have it
    let needsSave = false;
    this.items.forEach((item) => {
      if (!item.priceHistory) {
        item.priceHistory = [
          {
            date: item.dateAdded,
            price: item.currentPrice,
          },
        ];
        needsSave = true;
      }
      // Add lastPriceUpdate for backward compatibility
      if (!item.lastPriceUpdate) {
        item.lastPriceUpdate = item.dateAdded;
        needsSave = true;
      }
    });

    // Save migrated data if needed
    if (needsSave) {
      await this.saveItems();
    }

    this.filteredItems = [...this.items];
  }

  filterItems(searchTerm) {
    const term = searchTerm.toLowerCase();
    this.filteredItems = this.items.filter(
      (item) =>
        item.variantName.toLowerCase().includes(term) ||
        item.productName.toLowerCase().includes(term),
    );
    this.renderItems();
  }

  renderItems() {
    const grid = document.getElementById("items-grid");
    const emptyState = document.getElementById("empty-state");

    if (this.items.length === 0) {
      grid.style.display = "none";
      emptyState.style.display = "block";
      return;
    }

    emptyState.style.display = "none";
    grid.style.display = "grid";
    grid.innerHTML = "";

    this.filteredItems.forEach((item) => {
      const card = this.createItemCard(item);
      grid.appendChild(card);
    });
  }

  createItemCard(item) {
    const card = document.createElement("div");
    const priceDiff = item.currentPrice - item.targetPrice;
    const isReached = priceDiff <= 0;
    const hasError = item.error;

    card.className = `item-card ${isReached ? "price-reached" : ""} ${hasError ? "error" : ""}`;

    const diffText = isReached
      ? `Target reached! ${Math.abs(priceDiff).toFixed(2)} ${item.currency} below target`
      : `${priceDiff.toFixed(2)} ${item.currency} above target`;

    const diffClass = isReached ? "reached" : "above";

    card.innerHTML = `
      <div class="item-header">
        <div>
          <div class="item-name">${item.variantName}</div>
          <div class="item-product">${item.productName}</div>
          <div class="item-domain">${this.extractDomain(item.url)}</div>
        </div>
      </div>

      <div class="price-info">
        <div class="price-row">
          <span class="price-label">Current Price:</span>
          <span class="price-value current">${item.currentPrice} ${item.currency}</span>
        </div>
        <div class="price-row">
          <span class="price-label">Target Price:</span>
          <span class="price-value target">${item.targetPrice} ${item.currency}</span>
        </div>
      </div>

      <div class="price-difference ${diffClass}">
        ${diffText}
      </div>

      ${hasError ? `<div class="error-message">Error: ${item.error}</div>` : ""}

      <div class="item-meta">
        <div>Added: ${new Date(item.dateAdded).toLocaleDateString()}</div>
        <div>Last checked: ${this.formatRelativeTime(new Date(item.lastChecked))}</div>
        <div>Price updated: ${item.lastPriceUpdate ? this.formatRelativeTime(new Date(item.lastPriceUpdate)) : this.formatRelativeTime(new Date(item.dateAdded))}</div>
        <div>SKU: ${item.sku}</div>
      </div>

      <div class="price-history">
        <div class="history-title">Price History</div>
        ${this.renderPriceHistory(item)}
      </div>

      <div class="item-actions">
        <button class="btn btn-small secondary visit-btn" data-item-id="${item.id}">
          Visit Product
        </button>
        <button class="btn btn-small secondary edit-btn" data-item-id="${item.id}">
          Edit Target
        </button>
        <button class="btn btn-small danger remove-btn" data-item-id="${item.id}">
          Remove
        </button>
      </div>
    `;

    // Add event listeners for action buttons
    const visitBtn = card.querySelector(".visit-btn");
    const editBtn = card.querySelector(".edit-btn");
    const removeBtn = card.querySelector(".remove-btn");

    visitBtn.addEventListener("click", () => this.visitProduct(item.id));
    editBtn.addEventListener("click", () => this.editTarget(item.id));
    removeBtn.addEventListener("click", () => this.removeItem(item.id));

    // Add hover events for chart tooltips
    this.setupChartTooltips(card);

    return card;
  }

  renderPriceHistory(item) {
    if (!item.priceHistory || item.priceHistory.length < 2) {
      const firstPrice =
        item.priceHistory && item.priceHistory.length > 0
          ? item.priceHistory[0].price
          : item.currentPrice;

      return `
        <div class="no-history">
          First tracked at ${firstPrice} ${item.currency}
          <br>
          <small>Price history will appear after price changes are detected</small>
        </div>
      `;
    }

    const chartSvg = this.createPriceChart(item.priceHistory, item.currency);
    const stats = this.calculateHistoryStats(item.priceHistory, item.currency);

    return `
      <div class="price-chart">
        ${chartSvg}
      </div>
      <div class="history-stats">
        <div class="history-stat">
          <div class="history-stat-value">${stats.firstPrice}</div>
          <div class="history-stat-label">First Price</div>
        </div>
        <div class="history-stat">
          <div class="history-stat-value">${stats.lowestPrice}</div>
          <div class="history-stat-label">Lowest Price</div>
        </div>
      </div>
    `;
  }

  createPriceChart(priceHistory, currency) {
    const width = 268;
    const height = 100;
    const padding = 20;
    const chartId = `chart-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

    const prices = priceHistory.map((p) => p.price);
    const minPrice = Math.min(...prices);
    const maxPrice = Math.max(...prices);
    const priceRange = maxPrice - minPrice || 1; // Avoid division by zero

    // Create SVG points
    const points = priceHistory
      .map((point, index) => {
        const x =
          padding + (index / (priceHistory.length - 1)) * (width - 2 * padding);
        const y =
          height -
          padding -
          ((point.price - minPrice) / priceRange) * (height - 2 * padding);
        return `${x},${y}`;
      })
      .join(" ");

    // Create grid lines
    const gridLines = [];
    for (let i = 1; i <= 3; i++) {
      const y = padding + (i / 4) * (height - 2 * padding);
      gridLines.push(
        `<line x1="${padding}" y1="${y}" x2="${width - padding}" y2="${y}" stroke="#e0e0e0" stroke-width="1"/>`,
      );
    }

    // Create circles with hover events
    const circles = priceHistory
      .map((point, index) => {
        const x =
          padding + (index / (priceHistory.length - 1)) * (width - 2 * padding);
        const y =
          height -
          padding -
          ((point.price - minPrice) / priceRange) * (height - 2 * padding);

        const date = new Date(point.date).toLocaleDateString();
        const price = `${point.price.toFixed(2)} ${currency}`;

        return `
          <circle
            cx="${x}"
            cy="${y}"
            r="3"
            fill="#1a73e8"
            class="chart-point"
            data-date="${date}"
            data-price="${price}"
            style="cursor: pointer;"
          />`;
      })
      .join("");

    return `
      <div class="chart-container" style="position: relative;">
        <svg width="100%" height="100%" viewBox="0 0 ${width} ${height}" id="${chartId}">
          ${gridLines.join("")}
          <polyline
            fill="none"
            stroke="#1a73e8"
            stroke-width="2"
            points="${points}"
          />
          ${circles}
          <text x="${padding}" y="15" font-size="10" fill="#666">${maxPrice.toFixed(2)} ${currency}</text>
          <text x="${padding}" y="${height - 5}" font-size="10" fill="#666">${minPrice.toFixed(2)} ${currency}</text>
        </svg>
        <div class="chart-tooltip" style="
          position: absolute;
          background: rgba(0, 0, 0, 0.8);
          color: white;
          padding: 8px 12px;
          border-radius: 4px;
          font-size: 12px;
          pointer-events: none;
          opacity: 0;
          transition: opacity 0.2s;
          white-space: nowrap;
          z-index: 1000;
        "></div>
      </div>
    `;
  }

  calculateHistoryStats(priceHistory, currency) {
    const prices = priceHistory.map((p) => p.price);
    const firstPrice = prices[0];
    const lowestPrice = Math.min(...prices);

    return {
      firstPrice: `${firstPrice.toFixed(2)} ${currency}`,
      lowestPrice: `${lowestPrice.toFixed(2)} ${currency}`,
    };
  }

  extractDomain(url) {
    try {
      const domain = new URL(url).hostname;
      return domain.replace("www.", "");
    } catch (error) {
      return "Unknown";
    }
  }

  async visitProduct(itemId) {
    const item = this.items.find((i) => i.id === itemId);
    if (item) {
      await browser.tabs.create({ url: item.url });
    }
  }

  async editTarget(itemId) {
    const item = this.items.find((i) => i.id === itemId);
    if (!item) return;

    const newTarget = prompt(
      `Enter new target price for ${item.variantName}:`,
      item.targetPrice.toString(),
    );

    if (newTarget !== null && !isNaN(parseFloat(newTarget))) {
      item.targetPrice = parseFloat(newTarget);
      await this.saveItems();
      this.renderItems();
      this.updateStats();
    }
  }

  async removeItem(itemId) {
    if (confirm("Are you sure you want to remove this item from tracking?")) {
      this.items = this.items.filter((item) => item.id !== itemId);
      this.filteredItems = this.filteredItems.filter(
        (item) => item.id !== itemId,
      );
      await this.saveItems();
      this.renderItems();
      this.updateStats();
    }
  }

  async clearAllItems() {
    if (
      confirm(
        "Are you sure you want to remove ALL tracked items? This cannot be undone.",
      )
    ) {
      this.items = [];
      this.filteredItems = [];
      await this.saveItems();
      this.renderItems();
      this.updateStats();
    }
  }

  async checkAllPrices() {
    const button = document.getElementById("check-prices");
    button.textContent = "Checking...";
    button.disabled = true;

    try {
      // Send message to background script to check prices
      const response = await browser.runtime.sendMessage({
        action: "checkPricesNow",
      });

      if (response && response.success) {
        // Reload items after check
        setTimeout(async () => {
          await this.loadItems();
          this.renderItems();
          this.updateStats();
          button.textContent = "Check Prices Now";
          button.disabled = false;
        }, 1000);
      } else {
        throw new Error(response?.error || "Unknown error");
      }
    } catch (error) {
      console.error("Error checking prices:", error);
      button.textContent = "Check Prices Now";
      button.disabled = false;
    }
  }

  exportData() {
    const dataStr = JSON.stringify(this.items, null, 2);
    const dataBlob = new Blob([dataStr], { type: "application/json" });
    const url = URL.createObjectURL(dataBlob);

    const a = document.createElement("a");
    a.href = url;
    a.download = `price-tracker-data-${new Date().toISOString().split("T")[0]}.json`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  updateStats() {
    const totalItems = this.items.length;
    const targetsReached = this.items.filter(
      (item) => item.currentPrice <= item.targetPrice,
    ).length;

    let avgDiscount = 0;
    if (totalItems > 0) {
      const totalDiscount = this.items.reduce((sum, item) => {
        const discount =
          ((item.currentPrice - item.targetPrice) / item.currentPrice) * 100;
        return sum + discount;
      }, 0);
      avgDiscount = Math.round(totalDiscount / totalItems);
    }

    const lastCheck =
      this.items.length > 0
        ? new Date(
            Math.max(...this.items.map((item) => new Date(item.lastChecked))),
          )
        : null;

    document.getElementById("total-items").textContent = totalItems;
    document.getElementById("targets-reached").textContent = targetsReached;
    document.getElementById("avg-discount").textContent = `${avgDiscount}%`;
    document.getElementById("last-check").textContent = lastCheck
      ? this.formatRelativeTime(lastCheck)
      : "-";
  }

  setupChartTooltips(card) {
    const chartPoints = card.querySelectorAll(".chart-point");
    const tooltip = card.querySelector(".chart-tooltip");

    if (!tooltip || chartPoints.length === 0) return;

    chartPoints.forEach((point) => {
      point.addEventListener("mouseenter", (e) => {
        const date = e.target.getAttribute("data-date");
        const price = e.target.getAttribute("data-price");

        tooltip.innerHTML = `${date}<br>${price}`;
        tooltip.style.opacity = "1";

        // Position tooltip
        const rect = e.target.getBoundingClientRect();
        const containerRect = tooltip.parentElement.getBoundingClientRect();

        tooltip.style.left = `${rect.left - containerRect.left + rect.width / 2}px`;
        tooltip.style.top = `${rect.top - containerRect.top - 40}px`;
        tooltip.style.transform = "translateX(-50%)";
      });

      point.addEventListener("mouseleave", () => {
        tooltip.style.opacity = "0";
      });
    });
  }

  async saveItems() {
    await browser.storage.local.set({ trackedItems: this.items });
  }

  formatRelativeTime(date) {
    const now = new Date();
    const diffMs = now - date;
    const diffMinutes = Math.floor(diffMs / (1000 * 60));
    const diffHours = Math.floor(diffMs / (1000 * 60 * 60));
    const diffDays = Math.floor(diffMs / (1000 * 60 * 60 * 24));

    if (diffMinutes < 1) {
      return "just now";
    } else if (diffMinutes < 60) {
      return `${diffMinutes}m ago`;
    } else if (diffHours < 24) {
      return `${diffHours}h ago`;
    } else if (diffDays < 7) {
      return `${diffDays}d ago`;
    } else {
      return date.toLocaleDateString();
    }
  }
}

// Initialize the manager
const itemsManager = new TrackedItemsManager();
