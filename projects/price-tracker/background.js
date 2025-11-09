// background.js - Updated to support all parsing methods including @graph

// Import common parser functionality for background script
class ProductDataParser {
  constructor() {}

  // Helper method to normalize URLs to fully qualified format
  normalizeUrl(url, baseUrl = null) {
    if (!url) return baseUrl || "";

    // If already a fully qualified URL, return as-is
    if (url.startsWith("http://") || url.startsWith("https://")) {
      return url;
    }

    // Return base URL
    return baseUrl || "";
  }

  // Parse Shopify meta variables from HTML text
  parseShopifyMeta(document) {
    const scripts = document.querySelectorAll("script");
    for (const script of scripts) {
      const matches = [...script.innerText.matchAll(/var meta = (.*);$/gm)];
      if (matches.length !== 1) continue;

      try {
        const match = JSON.parse(matches[0][1]);
        console.log("Found Shopify meta data:", match);

        return this.normalizeProductData({
          name: match.product.title || "",
          description: match.product.description || "",
          brand: match.product.vendor || "",
          image: null,
          url: "",
          currency: match.currency || "EUR",
          variants: match.product.variants.map((variant) => ({
            sku: variant.sku || "",
            price: variant.price / 100.0,
            name: variant.title || variant.name || "",
            availability: variant.available ? "in_stock" : "out_of_stock",
            url: "",
          })),
        });
      } catch (error) {
        console.warn("Error parsing Shopify meta:", error);
      }
    }
    return null;
  }

  // Parse <meta> tags for Open Graph and other product data
  parseMetaTags(document) {
    const tags = document.querySelectorAll("meta");
    let productData = {};
    let itemData = {};

    for (const tag of tags) {
      const val = tag.getAttribute("content");
      const property = tag.name || tag.getAttribute("property");

      switch (property) {
        case "og:title":
          productData.name = val;
          itemData.name = val;
          break;
        case "og:url":
          productData.url = val;
          break;
        case "og:description":
          productData.description = val;
          break;
        case "og:image":
          productData.image = val;
          break;
        case "og:price:amount":
          itemData.price = parseFloat(val);
          break;
        case "og:price:currency":
          productData.currency = val;
          break;
        case "product:price:amount":
          itemData.price = parseFloat(val);
          break;
        case "product:price:currency":
          productData.currency = val;
          break;
      }
    }

    if (itemData.name || itemData.price) {
      return this.normalizeProductData({
        ...productData,
        variants: [
          {
            ...itemData,
            sku: "",
            availability: "unknown",
            url: this.normalizeUrl(productData.url),
          },
        ],
      });
    }

    return null;
  }

  // Extract items from JSON-LD data, handling @graph structures
  extractItemsFromJsonLd(data) {
    const items = [];

    if (Array.isArray(data)) {
      for (const item of data) {
        items.push(...this.extractItemsFromJsonLd(item));
      }
    } else if (data["@graph"] && Array.isArray(data["@graph"])) {
      for (const graphItem of data["@graph"]) {
        items.push(graphItem);
      }
    } else if (data["@type"] || data.hasVariant) {
      items.push(data);
    }

    return items;
  }

  // Check if the data contains product information
  isProductData(data) {
    const productTypes = ["Product", "ProductGroup"];
    return (
      productTypes.includes(data["@type"]) ||
      (data.hasVariant && Array.isArray(data.hasVariant))
    );
  }

  // Normalize product data to a consistent format
  normalizeProductData(data) {
    console.log("Normalizing product data:", data);
    if (!data) {
      return null;
    }

    const baseProduct = {
      name: data.name || "",
      description: data.description || "",
      brand: data.brand?.name || data.brand || "",
      image: data.image || "",
      url: this.normalizeUrl(data.url),
      currency: this.extractCurrency(data),
      variants: [],
    };

    // Handle pre-normalized variants array
    if (data.variants && Array.isArray(data.variants)) {
      baseProduct.variants = data.variants.map((variant) => ({
        sku: variant.sku || "",
        name: variant.name || baseProduct.name,
        size: variant.size || "",
        color: variant.color || "",
        price: variant.price || this.extractPrice(variant.offers),
        availability:
          variant.availability || this.extractAvailability(variant.offers),
        url: this.normalizeUrl(variant.url || variant.offers?.url || data.url),
      }));
    }
    // Handle JSON-LD single product
    else if (data["@type"] === "Product" && data.offers) {
      console.log("Extracting price from offers:", data.offers);
      baseProduct.variants.push({
        sku: data.sku || "",
        name: data.name || "",
        size: data.size || "",
        color: data.color || "",
        price: this.extractPrice(data.offers),
        availability: this.extractAvailability(data.offers),
        url: this.normalizeUrl(data.offers.url || data.url),
      });
    }
    // Handle JSON-LD product group with variants
    else if (data.hasVariant && Array.isArray(data.hasVariant)) {
      baseProduct.variants = data.hasVariant.map((variant) => ({
        sku: variant.sku || "",
        name: variant.name || baseProduct.name,
        size: variant.size || "",
        color: variant.color || "",
        price: this.extractPrice(variant.offers),
        availability: this.extractAvailability(variant.offers),
        url: this.normalizeUrl(variant.offers?.url || data.url),
      }));
    }

    if (baseProduct.variants.length === 0) {
      console.log("No variants found, returning null");
      return null;
    }

    return baseProduct;
  }

  // Extract price from offers object or direct value
  extractPrice(offers) {
    console.log("Extracting price from offers:", offers);
    if (typeof offers === "number") return offers;
    if (!offers) return null;

    const offer = Array.isArray(offers) ? offers[0] : offers;
    if (offer.price) return parseFloat(offer.price);
    if (offer.priceSpecification && Array.isArray(offer.priceSpecification)) {
      return parseFloat(offer.priceSpecification[0].price) || null;
    }

    return null;
  }

  // Extract currency from data
  extractCurrency(data) {
    // Check direct currency property
    if (data.currency) return data.currency;

    // Check offers
    const offers = data.offers || data.hasVariant?.[0]?.offers;
    if (!offers) return "EUR";

    const offer = Array.isArray(offers) ? offers[0] : offers;
    return offer.priceCurrency || "EUR";
  }

  // Extract availability from offers object
  extractAvailability(offers) {
    if (!offers) return "unknown";

    const offer = Array.isArray(offers) ? offers[0] : offers;
    const availability = offer.availability || "";

    if (availability.includes("InStock")) return "in_stock";
    if (availability.includes("OutOfStock")) return "out_of_stock";
    if (availability.includes("LimitedAvailability")) return "limited";

    return "unknown";
  }

  // Extract price from structured data
  extractPriceFromData(data, targetSku) {
    const items = this.extractItemsFromJsonLd(data);

    for (const item of items) {
      // Handle ProductGroup with variants
      if (item.hasVariant && Array.isArray(item.hasVariant)) {
        for (const variant of item.hasVariant) {
          if (variant.sku === targetSku && variant.offers) {
            return this.extractPrice(variant.offers);
          }
        }
      }

      // Handle single Product
      if (
        item["@type"] === "Product" &&
        item.sku === targetSku &&
        item.offers
      ) {
        return this.extractPrice(item.offers);
      }
    }

    return null;
  }
}

class PriceMonitor extends ProductDataParser {
  constructor() {
    super();
    this.isCheckingPrices = false;
    this.setupMessageListeners();
    // Initialize alarms and perform first check
    this.initialize();
  }

  async initialize() {
    await this.initializeAlarms();
    // Perform initial price check
    await this.checkAllPrices();
  }

  async initializeAlarms() {
    // Clear any existing alarms first
    await browser.alarms.clear("priceCheck");

    // Load user's preferred check interval
    const result = await browser.storage.local.get(["checkInterval"]);
    const interval = result.checkInterval || 6; // 6 hours default

    // Create alarm with user's preferred interval
    browser.alarms.create("priceCheck", {
      delayInMinutes: 1, // First check after 1 minute
      periodInMinutes: interval * 60,
    });

    // Listen for alarm events
    browser.alarms.onAlarm.addListener((alarm) => {
      if (alarm.name === "priceCheck") {
        console.log(
          `[${new Date().toISOString()}] Alarm triggered price check`,
        );
        this.checkAllPrices().catch((error) => {
          console.error("Failed to check prices on alarm:", error);
        });
      }
    });

    // Log alarm creation for debugging
    console.log(
      `[${new Date().toISOString()}] Price check alarm created with ${interval}h interval`,
    );
  }

  setupMessageListeners() {
    browser.runtime.onMessage.addListener((message, sender, sendResponse) => {
      if (message.action === "updateCheckInterval") {
        this.updateCheckInterval(message.interval);
        sendResponse({ success: true });
        return false;
      } else if (message.action === "checkPricesNow") {
        console.log(
          `[${new Date().toISOString()}] Manual price check triggered`,
        );
        // Handle async operation properly
        this.checkAllPrices()
          .then(() => {
            sendResponse({ success: true });
          })
          .catch((error) => {
            sendResponse({ success: false, error: error.message });
          });
        return true; // Keep message channel open for async response
      } else if (message.action === "contentScriptReady") {
        // Content script is ready, no response needed
        return false;
      }
      return false; // Close message channel for unhandled messages
    });
  }

  async updateCheckInterval(hours) {
    console.log(`Updating check interval to ${hours} hours`);
    // Clear existing alarm
    await browser.alarms.clear("priceCheck");

    // Create new alarm with updated interval
    browser.alarms.create("priceCheck", {
      delayInMinutes: hours * 60,
      periodInMinutes: hours * 60,
    });

    console.log(`New alarm created with ${hours}h interval`);
  }

  async checkAllPrices() {
    const now = new Date().toISOString();

    try {
      // Check if we've run recently to prevent excessive checking
      const result = await browser.storage.local.get([
        "trackedItems",
        "lastPriceCheck",
      ]);
      const trackedItems = result.trackedItems || [];
      const lastCheck = result.lastPriceCheck;

      // Rate limiting: don't check more than once every 30 minutes
      if (lastCheck) {
        const timeSinceLastCheck = Date.now() - new Date(lastCheck).getTime();
        const thirtyMinutes = 30 * 60 * 1000;

        if (timeSinceLastCheck < thirtyMinutes) {
          console.log(
            `[${now}] Skipping price check - last check was ${Math.round(timeSinceLastCheck / 60000)} minutes ago`,
          );
          return;
        }
      }

      if (trackedItems.length === 0) {
        console.log(`[${now}] No items to check`);
        return;
      }

      // Record when we started this check
      await browser.storage.local.set({ lastPriceCheck: now });

      const updatedItems = [];
      let alertCount = 0;

      for (const item of trackedItems) {
        try {
          const updatedItem = await this.checkItemPrice(item);
          updatedItems.push(updatedItem);

          // Check if target price is reached
          if (
            updatedItem.currentPrice <= updatedItem.targetPrice &&
            updatedItem.notifications &&
            updatedItem.currentPrice !== item.currentPrice
          ) {
            this.sendPriceAlert(updatedItem);
            alertCount++;
          }
        } catch (error) {
          console.error(`Error checking price for ${item.variantName}:`, error);
          // Keep original item if check fails
          updatedItems.push({
            ...item,
            lastChecked: new Date().toISOString(),
            error: error.message,
          });
        }
      }

      // Save updated items
      await browser.storage.local.set({ trackedItems: updatedItems });

      console.log(
        `Price check completed. ${alertCount} alerts sent for ${updatedItems.length} items.`,
      );
    } catch (error) {
      console.error("Error in checkAllPrices:", error);
    }
  }

  async checkItemPrice(item) {
    try {
      // Ensure item has price history (backward compatibility)
      if (!item.priceHistory) {
        item.priceHistory = [
          {
            date: item.dateAdded,
            price: item.currentPrice,
          },
        ];
      }

      // Fetch the product page
      const response = await fetch(item.url, {
        headers: {
          "User-Agent":
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:91.0) Gecko/20100101 Firefox/91.0",
        },
      });

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const html = await response.text();
      const parser = new DOMParser();
      const doc = parser.parseFromString(html, "text/html");

      // Try to find the current price using all available parsing methods
      let currentPrice = null;

      // Method 1: Try JSON-LD parsing
      currentPrice = this.extractPriceFromJsonLD(doc, item.sku);

      // Method 2: Try Shopify meta parsing if JSON-LD failed
      if (currentPrice === null) {
        currentPrice = this.extractPriceFromShopify(doc, item.sku);
      }

      // Method 3: Try meta tags parsing if others failed
      if (currentPrice === null) {
        currentPrice = this.extractPriceFromMetaTags(doc, item.sku);
      }

      const updatedItem = {
        ...item,
        currentPrice: currentPrice !== null ? currentPrice : item.currentPrice,
        lastChecked: new Date().toISOString(),
        error: currentPrice === null ? "Price not found" : null,
      };

      if (currentPrice !== null) {
        const now = new Date().toISOString();
        updatedItem.lastPriceUpdate = now;
        updatedItem.priceHistory = [
          ...item.priceHistory,
          {
            date: now,
            price: currentPrice,
          },
        ];

        // Limit history to last 100 entries to prevent storage bloat
        if (updatedItem.priceHistory.length > 100) {
          updatedItem.priceHistory = updatedItem.priceHistory.slice(-100);
        }
      }

      return updatedItem;
    } catch (error) {
      throw new Error(`Failed to fetch price: ${error.message}`);
    }
  }

  // Extract price from JSON-LD data with @graph support
  extractPriceFromJsonLD(doc, targetSku) {
    const jsonLdScripts = doc.querySelectorAll(
      'script[type="application/ld+json"]',
    );

    for (const script of jsonLdScripts) {
      try {
        const data = JSON.parse(script.textContent);
        console.log("Parsing JSON-LD data for price extraction:", data);

        const price = this.extractPriceFromData(data, targetSku);
        if (price !== null) {
          console.log(`Found price ${price} for SKU ${targetSku}`);
          return price;
        }
      } catch (error) {
        console.warn("Error parsing JSON-LD:", error);
      }
    }

    return null;
  }

  // Extract price from Shopify meta data
  extractPriceFromShopify(doc, targetSku) {
    try {
      const productData = this.parseShopifyMeta(doc);
      if (!productData || !productData.variants) return null;

      // Find variant by SKU
      const variant = productData.variants.find((v) => v.sku === targetSku);
      return variant ? variant.price : null;
    } catch (error) {
      console.warn("Error parsing Shopify meta:", error);
      return null;
    }
  }

  // Extract price from meta tags
  extractPriceFromMetaTags(doc, targetSku) {
    try {
      const productData = this.parseMetaTags(doc);
      if (!productData || !productData.variants) return null;

      // For meta tags, we usually get a single variant
      // If no SKU matching is needed, return the first variant price
      if (productData.variants.length === 1) {
        return productData.variants[0].price;
      }

      // Try to find by SKU if multiple variants
      const variant = productData.variants.find((v) => v.sku === targetSku);
      return variant ? variant.price : null;
    } catch (error) {
      console.warn("Error parsing meta tags:", error);
      return null;
    }
  }

  sendPriceAlert(item) {
    const title = "Price Drop Alert!";
    const message = `${item.variantName} is now ${item.currentPrice} ${item.currency} (target: ${item.targetPrice} ${item.currency})`;

    browser.notifications.create({
      type: "basic",
      iconUrl: "icons/icon-48.png",
      title: title,
      message: message,
      buttons: [{ title: "View Product" }, { title: "Dismiss" }],
    });

    // Handle notification clicks
    browser.notifications.onButtonClicked.addListener(
      (notificationId, buttonIndex) => {
        if (buttonIndex === 0) {
          // Open product page
          browser.tabs.create({ url: item.url });
        }
        browser.notifications.clear(notificationId);
      },
    );

    browser.notifications.onClicked.addListener((notificationId) => {
      browser.tabs.create({ url: item.url });
      browser.notifications.clear(notificationId);
    });
  }
}

// Initialize price monitor
const priceMonitor = new PriceMonitor();

// Handle extension install/update
browser.runtime.onInstalled.addListener((details) => {
  if (details.reason === "install") {
    console.log("Price Tracker extension installed");
  } else if (details.reason === "update") {
    console.log("Price Tracker extension updated");
  }
});
