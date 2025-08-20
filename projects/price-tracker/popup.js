// popup.js - Main popup logic

class PriceTrackerPopup {
  constructor() {
    this.currentProduct = null;
    this.selectedVariant = null;
    this.trackedItems = [];

    this.init();
  }

  async init() {
    await this.loadTrackedItems();
    await this.loadSettings();
    this.setupEventListeners();
    await this.loadProductData();
    this.renderTrackedItems();
  }

  setupEventListeners() {
    document
      .getElementById("track-btn")
      .addEventListener("click", () => this.trackSelectedVariant());
    document
      .getElementById("check-interval")
      .addEventListener("change", (e) => this.saveSettings());
    document
      .getElementById("view-all-btn")
      .addEventListener("click", () => this.openAllTrackedItems());
    document
      .getElementById("check-now-btn")
      .addEventListener("click", () => this.checkPricesNow());
  }

  async loadProductData() {
    try {
      const tabs = await browser.tabs.query({
        active: true,
        currentWindow: true,
      });
      const response = await browser.tabs.sendMessage(tabs[0].id, {
        action: "getProductData",
      });

      if (
        response &&
        response.productData &&
        response.productData.variants.length > 0
      ) {
        this.currentProduct = response.productData;
        this.renderProduct();
        this.hideElement("loading");
        this.showElement("product-content");
      } else {
        this.showNoProduct();
      }
    } catch (error) {
      console.error("Error loading product data:", error);
      this.showError(
        "Failed to load product data. Make sure the page has finished loading.",
      );
    }
  }

  renderProduct() {
    document.getElementById("product-name").textContent =
      this.currentProduct.name;
    document.getElementById("product-meta").textContent =
      `${this.currentProduct.brand} • ${this.currentProduct.variants.length} variant(s)`;

    this.renderVariants();
  }

  renderVariants() {
    const variantList = document.getElementById("variant-list");
    variantList.innerHTML = "";

    this.currentProduct.variants.forEach((variant, index) => {
      const variantElement = this.createVariantElement(variant, index);
      variantList.appendChild(variantElement);
    });
  }

  createVariantElement(variant, index) {
    const div = document.createElement("div");
    div.className = "variant-item";
    div.dataset.index = index;

    const displayName = this.getVariantDisplayName(variant);
    const price = variant.price
      ? `${variant.price} ${this.currentProduct.currency}`
      : "Price not available";

    div.innerHTML = `
      <div class="variant-name">${displayName}</div>
      <div class="variant-details">
        <span class="variant-price">${price}</span>
        <span class="availability ${variant.availability}">${this.formatAvailability(variant.availability)}</span>
      </div>
    `;

    div.addEventListener("click", () => this.selectVariant(index));
    return div;
  }

  getVariantDisplayName(variant) {
    const parts = [];
    if (variant.color) parts.push(variant.color);
    if (variant.size) parts.push(variant.size);

    if (parts.length > 0) {
      return `${variant.name} (${parts.join(", ")})`;
    }
    return variant.name;
  }

  formatAvailability(availability) {
    switch (availability) {
      case "in_stock":
        return "In Stock";
      case "out_of_stock":
        return "Out of Stock";
      case "limited":
        return "Limited";
      default:
        return "Unknown";
    }
  }

  selectVariant(index) {
    // Remove previous selection
    document.querySelectorAll(".variant-item").forEach((item) => {
      item.classList.remove("selected");
    });

    // Select new variant
    const variantElement = document.querySelector(
      `.variant-item[data-index="${index}"]`,
    );
    variantElement.classList.add("selected");

    this.selectedVariant = this.currentProduct.variants[index];

    // Show tracking section with current price as default
    const trackSection = document.getElementById("track-section");
    const targetPriceInput = document.getElementById("target-price");

    if (this.selectedVariant.price) {
      targetPriceInput.value = (this.selectedVariant.price * 0.9).toFixed(2); // 10% discount as default
    }

    this.showElement("track-section");
  }

  async trackSelectedVariant() {
    if (!this.selectedVariant) return;

    const targetPrice = parseFloat(
      document.getElementById("target-price").value,
    );
    if (!targetPrice || targetPrice <= 0) {
      this.showError("Please enter a valid target price.");
      return;
    }

    const trackedItem = {
      id: this.generateId(),
      url: this.selectedVariant.url,
      productName: this.currentProduct.name,
      variantName: this.getVariantDisplayName(this.selectedVariant),
      sku: this.selectedVariant.sku,
      currentPrice: this.selectedVariant.price,
      targetPrice: targetPrice,
      currency: this.currentProduct.currency,
      dateAdded: new Date().toISOString(),
      lastChecked: new Date().toISOString(),
      notifications: true,
    };

    this.trackedItems.push(trackedItem);
    await this.saveTrackedItems();

    this.renderTrackedItems();
    this.hideElement("track-section");
    this.clearVariantSelection();

    // Show success message
    this.showError("Item added to tracking!", "success");
    setTimeout(() => this.hideElement("error"), 3000);
  }

  async removeTrackedItem(id) {
    this.trackedItems = this.trackedItems.filter((item) => item.id !== id);
    await this.saveTrackedItems();
    this.renderTrackedItems();
  }

  renderTrackedItems() {
    const trackedList = document.getElementById("tracked-list");
    const trackedSection = document.getElementById("tracked-items");

    if (this.trackedItems.length === 0) {
      trackedList.innerHTML =
        '<div style="text-align: center; color: #666; padding: 20px;">No items tracked yet</div>';
      return;
    }

    trackedList.innerHTML = "";
    this.trackedItems.forEach((item) => {
      const itemElement = this.createTrackedItemElement(item);
      trackedList.appendChild(itemElement);
    });
  }

  createTrackedItemElement(item) {
    const div = document.createElement("div");
    div.className = "tracked-item";

    const priceDiff = item.currentPrice - item.targetPrice;
    const diffText =
      priceDiff > 0
        ? `(${priceDiff.toFixed(2)} above target)`
        : "(target reached!)";
    const diffColor = priceDiff > 0 ? "#d93025" : "#137333";

    div.innerHTML = `
      <div class="tracked-item-name">${item.variantName}</div>
      <div class="tracked-item-details">
        <span>Current: ${item.currentPrice} ${item.currency}</span>
        <span>Target: ${item.targetPrice} ${item.currency}</span>
      </div>
      <div style="font-size: 11px; color: ${diffColor}; margin-top: 4px;">${diffText}</div>
      <button class="remove-btn" data-item-id="${item.id}">Remove</button>
    `;

    // Add event listener to remove button
    const removeBtn = div.querySelector(".remove-btn");
    removeBtn.addEventListener("click", () => this.removeTrackedItem(item.id));

    return div;
  }

  clearVariantSelection() {
    document.querySelectorAll(".variant-item").forEach((item) => {
      item.classList.remove("selected");
    });
    this.selectedVariant = null;
  }

  async loadTrackedItems() {
    const result = await browser.storage.local.get(["trackedItems"]);
    this.trackedItems = result.trackedItems || [];
  }

  async saveTrackedItems() {
    await browser.storage.local.set({ trackedItems: this.trackedItems });
  }

  async loadSettings() {
    const result = await browser.storage.local.get(["checkInterval"]);
    const interval = result.checkInterval || 6;
    document.getElementById("check-interval").value = interval;
  }

  async saveSettings() {
    const interval = parseInt(document.getElementById("check-interval").value);
    await browser.storage.local.set({ checkInterval: interval });

    // Update alarm
    browser.runtime.sendMessage({
      action: "updateCheckInterval",
      interval: interval,
    });
  }

  openAllTrackedItems() {
    browser.tabs.create({
      url: browser.runtime.getURL("tracked-items.html"),
    });
  }

  async checkPricesNow() {
    const button = document.getElementById("check-now-btn");
    button.textContent = "Checking...";
    button.disabled = true;

    try {
      const response = await browser.runtime.sendMessage({
        action: "checkPricesNow",
      });

      if (response && response.success) {
        // Wait a moment for the background script to finish
        setTimeout(async () => {
          await this.loadTrackedItems();
          this.renderTrackedItems();
          button.textContent = "Check Prices Now";
          button.disabled = false;
          this.showError("Prices checked successfully!", "success");
          setTimeout(() => this.hideElement("error"), 2000);
        }, 1000);
      } else {
        throw new Error(response?.error || "Unknown error");
      }
    } catch (error) {
      console.error("Error checking prices:", error);
      button.textContent = "Check Prices Now";
      button.disabled = false;
      this.showError("Error checking prices. Please try again.");
    }
  }

  generateId() {
    return Date.now().toString(36) + Math.random().toString(36).substr(2);
  }

  showElement(elementId) {
    document.getElementById(elementId).classList.remove("hidden");
  }

  hideElement(elementId) {
    document.getElementById(elementId).classList.add("hidden");
  }

  showError(message, type = "error") {
    const errorElement = document.getElementById("error");
    errorElement.textContent = message;
    errorElement.className = type === "success" ? "error" : "error";
    errorElement.style.background = type === "success" ? "#e6f4ea" : "#fce8e6";
    errorElement.style.color = type === "success" ? "#137333" : "#d93025";
    this.showElement("error");
  }

  showNoProduct() {
    this.hideElement("loading");
    this.showElement("no-product");
  }
}

// Initialize popup
const popup = new PriceTrackerPopup();
