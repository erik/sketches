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
        <div>Last checked: ${new Date(item.lastChecked).toLocaleDateString()}</div>
        <div>SKU: ${item.sku}</div>
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

    return card;
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
      ? lastCheck.toLocaleDateString()
      : "-";
  }

  async saveItems() {
    await browser.storage.local.set({ trackedItems: this.items });
  }
}

// Initialize the manager
const itemsManager = new TrackedItemsManager();
