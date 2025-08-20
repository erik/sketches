// background.js - Updated to support all parsing methods

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

    if (baseProduct.variants.length === 0) {
      console.log("No variants found, returning null");
      return null;
    }

    return baseProduct;
  }

  // Extract price from offers object or direct value
  extractPrice(offers) {
    if (typeof offers === "number") return offers;
    if (!offers) return null;

    const offer = Array.isArray(offers) ? offers[0] : offers;
    return parseFloat(offer.price) || null;
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
    // Handle arrays of structured data
    const items = Array.isArray(data) ? data : [data];

    for (const item of items) {
      // Handle ProductGroup with variants
      if (item.hasVariant && Array.isArray(item.hasVariant)) {
        for (const variant of item.hasVariant) {
          if (variant.sku === targetSku && variant.offers) {
            return this.extractPriceFromOffer(variant.offers);
          }
        }
      }

      // Handle single Product
      if (
        item["@type"] === "Product" &&
        item.sku === targetSku &&
        item.offers
      ) {
        return this.extractPriceFromOffer(item.offers);
      }
    }

    return null;
  }

  // Extract price from offer object
  extractPriceFromOffer(offers) {
    const offer = Array.isArray(offers) ? offers[0] : offers;
    return parseFloat(offer.price) || null;
  }
}

class PriceMonitor extends ProductDataParser {
  constructor() {
    super();
    this.setupAlarms();
    this.setupMessageListeners();
  }

  setupAlarms() {
    // Create initial alarm
    browser.alarms.create("priceCheck", {
      delayInMinutes: 1,
      periodInMinutes: 360, // 6 hours default
    });

    // Listen for alarm events
    browser.alarms.onAlarm.addListener((alarm) => {
      if (alarm.name === "priceCheck") {
        this.checkAllPrices();
      }
    });
  }

  setupMessageListeners() {
    browser.runtime.onMessage.addListener((message, sender, sendResponse) => {
      if (message.action === "updateCheckInterval") {
        this.updateCheckInterval(message.interval);
        sendResponse({ success: true });
        return false;
      } else if (message.action === "checkPricesNow") {
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
    // Clear existing alarm
    browser.alarms.clear("priceCheck");

    // Create new alarm with updated interval
    browser.alarms.create("priceCheck", {
      delayInMinutes: 1,
      periodInMinutes: hours * 60,
    });
  }

  async checkAllPrices() {
    console.log("Checking prices for all tracked items...");

    try {
      const result = await browser.storage.local.get(["trackedItems"]);
      const trackedItems = result.trackedItems || [];

      if (trackedItems.length === 0) {
        console.log("No items to check");
        return;
      }

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

      console.log(`Price check complete. ${alertCount} alerts sent.`);
    } catch (error) {
      console.error("Error during price check:", error);
    }
  }

  async checkItemPrice(item) {
    try {
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

      // Method 1: Try JSON-LD parsing (existing method)
      currentPrice = this.extractPriceFromJsonLD(doc, item.sku);

      // Method 2: Try Shopify meta parsing if JSON-LD failed
      if (currentPrice === null) {
        currentPrice = this.extractPriceFromShopify(doc, item.sku);
      }

      // Method 3: Try meta tags parsing if others failed
      if (currentPrice === null) {
        currentPrice = this.extractPriceFromMetaTags(doc, item.sku);
      }

      return {
        ...item,
        currentPrice: currentPrice !== null ? currentPrice : item.currentPrice,
        lastChecked: new Date().toISOString(),
        error: currentPrice === null ? "Price not found" : null,
      };
    } catch (error) {
      throw new Error(`Failed to fetch price: ${error.message}`);
    }
  }

  // Extract price from JSON-LD data
  extractPriceFromJsonLD(doc, targetSku) {
    const jsonLdScripts = doc.querySelectorAll(
      'script[type="application/ld+json"]',
    );

    for (const script of jsonLdScripts) {
      try {
        const data = JSON.parse(script.textContent);
        const price = this.extractPriceFromData(data, targetSku);
        if (price !== null) return price;
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
