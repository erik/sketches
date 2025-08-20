// parser-common.js - Shared parsing logic for both content and background scripts

class ProductDataParser {
  constructor() {
    this.productData = null;
  }

  // Helper method to normalize URLs to fully qualified format
  normalizeUrl(url, baseUrl = null) {
    if (!url) return baseUrl || (typeof window !== 'undefined' ? window.location.href : '');

    // If already a fully qualified URL, return as-is
    if (url.startsWith("http://") || url.startsWith("https://")) {
      return url;
    }

    // Return base URL or current page
    return baseUrl || (typeof window !== 'undefined' ? window.location.href : '');
  }

  // Parse Shopify meta variables
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
          url: typeof window !== 'undefined' ? window.location.href : '',
          currency: match.currency || "EUR",
          variants: match.product.variants.map((variant) => ({
            sku: variant.sku || "",
            price: variant.price / 100.0,
            name: variant.title || variant.name || "",
            availability: variant.available ? "in_stock" : "out_of_stock",
            url: typeof window !== 'undefined' ? window.location.href : ''
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
        variants: [{
          ...itemData,
          sku: "",
          availability: "unknown",
          url: this.normalizeUrl(productData.url)
        }],
      });
    }

    return null;
  }

  // Parse JSON-LD structured data
  parseLinkedData(document) {
    const scripts = document.querySelectorAll('script[type="application/ld+json"]');
    let productData = null;

    for (const script of scripts) {
      try {
        const data = JSON.parse(script.textContent);
        console.log("Parsing JSON-LD data:", data);

        // Handle arrays of structured data
        const items = Array.isArray(data) ? data : [data];

        for (const item of items) {
          if (this.isProductData(item)) {
            productData = this.normalizeProductData(item);
            break;
          }
        }

        if (productData && productData.variants && productData.variants.length > 0) {
          break;
        }
      } catch (error) {
        console.warn("Error parsing JSON-LD:", error);
      }
    }

    return productData;
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
        availability: variant.availability || this.extractAvailability(variant.offers),
        url: this.normalizeUrl(variant.url || variant.offers?.url || data.url),
      }));
    }
    // Handle JSON-LD single product
    else if (data["@type"] === "Product" && data.offers) {
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
    if (typeof offers === 'number') return offers;
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

  // Extract price from structured data for background script
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
      if (item["@type"] === "Product" && item.sku === targetSku && item.offers) {
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

  // Parse all available data sources and return the best match
  parseAllSources(document) {
    // Try parsing methods in order of preference
    let productData = this.parseLinkedData(document);
    
    if (!productData || !productData.variants.length) {
      productData = this.parseShopifyMeta(document);
    }
    
    if (!productData || !productData.variants.length) {
      productData = this.parseMetaTags(document);
    }

    return productData;
  }
}