A long chain of aliases should produce an odocl file that's a reasonable
size.

  $ ocamlc -c -bin-annot chain.mli
  $ odoc compile chain.cmti
  $ odoc link chain.odoc -I .
  $ odoc_print -r M5 chain.odocl
  {
    "id": { "`Module": [ { "`Root": [ "None", "Chain" ] }, "M5" ] },
    "doc": [],
    "type_": {
      "Alias": [
        {
          "`Resolved": {
            "`Canonical": [
              {
                "`Alias": [
                  {
                    "`Alias": [
                      {
                        "`Canonical": [
                          {
                            "`Alias": [
                              {
                                "`Alias": [
                                  {
                                    "`Canonical": [
                                      {
                                        "`Alias": [
                                          {
                                            "`Alias": [
                                              {
                                                "`Canonical": [
                                                  {
                                                    "`Alias": [
                                                      {
                                                        "`Alias": [
                                                          {
                                                            "`Canonical": [
                                                              {
                                                                "`Alias": [
                                                                  {
                                                                    "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                    }
                                                                  },
                                                                  {
                                                                    "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                    }
                                                                  }
                                                                ]
                                                              },
                                                              {
                                                                "`Resolved": {
                                                                  "`Identifier": {
                                                                    "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                    ]
                                                                  }
                                                                }
                                                              }
                                                            ]
                                                          },
                                                          {
                                                            "`Identifier": {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Chain"
                                                                  ]
                                                                },
                                                                "M1"
                                                              ]
                                                            }
                                                          }
                                                        ]
                                                      },
                                                      {
                                                        "`Hidden": {
                                                          "`Identifier": {
                                                            "`Module": [
                                                              {
                                                                "`Root": [
                                                                  "None",
                                                                  "Chain"
                                                                ]
                                                              },
                                                              "{M2__}2"
                                                            ]
                                                          }
                                                        }
                                                      }
                                                    ]
                                                  },
                                                  {
                                                    "`Resolved": {
                                                      "`Identifier": {
                                                        "`Module": [
                                                          {
                                                            "`Root": [
                                                              "None", "Chain"
                                                            ]
                                                          },
                                                          "M2"
                                                        ]
                                                      }
                                                    }
                                                  }
                                                ]
                                              },
                                              {
                                                "`Identifier": {
                                                  "`Module": [
                                                    {
                                                      "`Root": [
                                                        "None", "Chain"
                                                      ]
                                                    },
                                                    "M2"
                                                  ]
                                                }
                                              }
                                            ]
                                          },
                                          {
                                            "`Hidden": {
                                              "`Identifier": {
                                                "`Module": [
                                                  {
                                                    "`Root": [
                                                      "None", "Chain"
                                                    ]
                                                  },
                                                  "{M3__}3"
                                                ]
                                              }
                                            }
                                          }
                                        ]
                                      },
                                      {
                                        "`Resolved": {
                                          "`Identifier": {
                                            "`Module": [
                                              { "`Root": [ "None", "Chain" ] },
                                              "M3"
                                            ]
                                          }
                                        }
                                      }
                                    ]
                                  },
                                  {
                                    "`Identifier": {
                                      "`Module": [
                                        { "`Root": [ "None", "Chain" ] },
                                        "M3"
                                      ]
                                    }
                                  }
                                ]
                              },
                              {
                                "`Hidden": {
                                  "`Identifier": {
                                    "`Module": [
                                      { "`Root": [ "None", "Chain" ] },
                                      "{M4__}4"
                                    ]
                                  }
                                }
                              }
                            ]
                          },
                          {
                            "`Resolved": {
                              "`Identifier": {
                                "`Module": [
                                  { "`Root": [ "None", "Chain" ] },
                                  "M4"
                                ]
                              }
                            }
                          }
                        ]
                      },
                      {
                        "`Identifier": {
                          "`Module": [ { "`Root": [ "None", "Chain" ] }, "M4" ]
                        }
                      }
                    ]
                  },
                  {
                    "`Hidden": {
                      "`Identifier": {
                        "`Module": [
                          { "`Root": [ "None", "Chain" ] },
                          "{M5__}5"
                        ]
                      }
                    }
                  }
                ]
              },
              {
                "`Resolved": {
                  "`Identifier": {
                    "`Module": [ { "`Root": [ "None", "Chain" ] }, "M5" ]
                  }
                }
              }
            ]
          }
        },
        {
          "Some": {
            "Signature": {
              "items": [
                {
                  "Module": [
                    "Ordinary",
                    {
                      "id": {
                        "`Module": [
                          {
                            "`Module": [
                              { "`Root": [ "None", "Chain" ] },
                              "M5"
                            ]
                          },
                          "B"
                        ]
                      },
                      "doc": [],
                      "type_": {
                        "Alias": [
                          {
                            "`Resolved": {
                              "`Alias": [
                                {
                                  "`Alias": [
                                    {
                                      "`Alias": [
                                        {
                                          "`Alias": [
                                            {
                                              "`Alias": [
                                                {
                                                  "`Alias": [
                                                    {
                                                      "`Alias": [
                                                        {
                                                          "`Alias": [
                                                            {
                                                              "`Module": [
                                                                {
                                                                  "`Identifier": {
                                                                    "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                    ]
                                                                  }
                                                                },
                                                                "B"
                                                              ]
                                                            },
                                                            {
                                                              "`Module": [
                                                                {
                                                                  "`Canonical": [
                                                                    {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                    }
                                                                  ]
                                                                },
                                                                "B"
                                                              ]
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Module": [
                                                            {
                                                              "`Alias": [
                                                                {
                                                                  "`Canonical": [
                                                                    {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                    }
                                                                  ]
                                                                },
                                                                {
                                                                  "`Identifier": {
                                                                    "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                    ]
                                                                  }
                                                                }
                                                              ]
                                                            },
                                                            "B"
                                                          ]
                                                        }
                                                      ]
                                                    },
                                                    {
                                                      "`Module": [
                                                        {
                                                          "`Canonical": [
                                                            {
                                                              "`Alias": [
                                                                {
                                                                  "`Alias": [
                                                                    {
                                                                      "`Canonical": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                    }
                                                                  ]
                                                                },
                                                                {
                                                                  "`Hidden": {
                                                                    "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M2__}2"
                                                                      ]
                                                                    }
                                                                  }
                                                                }
                                                              ]
                                                            },
                                                            {
                                                              "`Resolved": {
                                                                "`Identifier": {
                                                                  "`Module": [
                                                                    {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                    },
                                                                    "M2"
                                                                  ]
                                                                }
                                                              }
                                                            }
                                                          ]
                                                        },
                                                        "B"
                                                      ]
                                                    }
                                                  ]
                                                },
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Alias": [
                                                        {
                                                          "`Canonical": [
                                                            {
                                                              "`Alias": [
                                                                {
                                                                  "`Alias": [
                                                                    {
                                                                      "`Canonical": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                    }
                                                                  ]
                                                                },
                                                                {
                                                                  "`Hidden": {
                                                                    "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M2__}2"
                                                                      ]
                                                                    }
                                                                  }
                                                                }
                                                              ]
                                                            },
                                                            {
                                                              "`Resolved": {
                                                                "`Identifier": {
                                                                  "`Module": [
                                                                    {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                    },
                                                                    "M2"
                                                                  ]
                                                                }
                                                              }
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Identifier": {
                                                            "`Module": [
                                                              {
                                                                "`Root": [
                                                                  "None",
                                                                  "Chain"
                                                                ]
                                                              },
                                                              "M2"
                                                            ]
                                                          }
                                                        }
                                                      ]
                                                    },
                                                    "B"
                                                  ]
                                                }
                                              ]
                                            },
                                            {
                                              "`Module": [
                                                {
                                                  "`Canonical": [
                                                    {
                                                      "`Alias": [
                                                        {
                                                          "`Alias": [
                                                            {
                                                              "`Canonical": [
                                                                {
                                                                  "`Alias": [
                                                                    {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Canonical": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M2__}2"
                                                                      ]
                                                                      }
                                                                      }
                                                                    }
                                                                  ]
                                                                },
                                                                {
                                                                  "`Resolved": {
                                                                    "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M2"
                                                                      ]
                                                                    }
                                                                  }
                                                                }
                                                              ]
                                                            },
                                                            {
                                                              "`Identifier": {
                                                                "`Module": [
                                                                  {
                                                                    "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                    ]
                                                                  },
                                                                  "M2"
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Hidden": {
                                                            "`Identifier": {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Chain"
                                                                  ]
                                                                },
                                                                "{M3__}3"
                                                              ]
                                                            }
                                                          }
                                                        }
                                                      ]
                                                    },
                                                    {
                                                      "`Resolved": {
                                                        "`Identifier": {
                                                          "`Module": [
                                                            {
                                                              "`Root": [
                                                                "None", "Chain"
                                                              ]
                                                            },
                                                            "M3"
                                                          ]
                                                        }
                                                      }
                                                    }
                                                  ]
                                                },
                                                "B"
                                              ]
                                            }
                                          ]
                                        },
                                        {
                                          "`Module": [
                                            {
                                              "`Alias": [
                                                {
                                                  "`Canonical": [
                                                    {
                                                      "`Alias": [
                                                        {
                                                          "`Alias": [
                                                            {
                                                              "`Canonical": [
                                                                {
                                                                  "`Alias": [
                                                                    {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Canonical": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M2__}2"
                                                                      ]
                                                                      }
                                                                      }
                                                                    }
                                                                  ]
                                                                },
                                                                {
                                                                  "`Resolved": {
                                                                    "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M2"
                                                                      ]
                                                                    }
                                                                  }
                                                                }
                                                              ]
                                                            },
                                                            {
                                                              "`Identifier": {
                                                                "`Module": [
                                                                  {
                                                                    "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                    ]
                                                                  },
                                                                  "M2"
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Hidden": {
                                                            "`Identifier": {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Chain"
                                                                  ]
                                                                },
                                                                "{M3__}3"
                                                              ]
                                                            }
                                                          }
                                                        }
                                                      ]
                                                    },
                                                    {
                                                      "`Resolved": {
                                                        "`Identifier": {
                                                          "`Module": [
                                                            {
                                                              "`Root": [
                                                                "None", "Chain"
                                                              ]
                                                            },
                                                            "M3"
                                                          ]
                                                        }
                                                      }
                                                    }
                                                  ]
                                                },
                                                {
                                                  "`Identifier": {
                                                    "`Module": [
                                                      {
                                                        "`Root": [
                                                          "None", "Chain"
                                                        ]
                                                      },
                                                      "M3"
                                                    ]
                                                  }
                                                }
                                              ]
                                            },
                                            "B"
                                          ]
                                        }
                                      ]
                                    },
                                    {
                                      "`Module": [
                                        {
                                          "`Canonical": [
                                            {
                                              "`Alias": [
                                                {
                                                  "`Alias": [
                                                    {
                                                      "`Canonical": [
                                                        {
                                                          "`Alias": [
                                                            {
                                                              "`Alias": [
                                                                {
                                                                  "`Canonical": [
                                                                    {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Canonical": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M2__}2"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M2"
                                                                      ]
                                                                      }
                                                                      }
                                                                    }
                                                                  ]
                                                                },
                                                                {
                                                                  "`Identifier": {
                                                                    "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M2"
                                                                    ]
                                                                  }
                                                                }
                                                              ]
                                                            },
                                                            {
                                                              "`Hidden": {
                                                                "`Identifier": {
                                                                  "`Module": [
                                                                    {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                    },
                                                                    "{M3__}3"
                                                                  ]
                                                                }
                                                              }
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Resolved": {
                                                            "`Identifier": {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Chain"
                                                                  ]
                                                                },
                                                                "M3"
                                                              ]
                                                            }
                                                          }
                                                        }
                                                      ]
                                                    },
                                                    {
                                                      "`Identifier": {
                                                        "`Module": [
                                                          {
                                                            "`Root": [
                                                              "None", "Chain"
                                                            ]
                                                          },
                                                          "M3"
                                                        ]
                                                      }
                                                    }
                                                  ]
                                                },
                                                {
                                                  "`Hidden": {
                                                    "`Identifier": {
                                                      "`Module": [
                                                        {
                                                          "`Root": [
                                                            "None", "Chain"
                                                          ]
                                                        },
                                                        "{M4__}4"
                                                      ]
                                                    }
                                                  }
                                                }
                                              ]
                                            },
                                            {
                                              "`Resolved": {
                                                "`Identifier": {
                                                  "`Module": [
                                                    {
                                                      "`Root": [
                                                        "None", "Chain"
                                                      ]
                                                    },
                                                    "M4"
                                                  ]
                                                }
                                              }
                                            }
                                          ]
                                        },
                                        "B"
                                      ]
                                    }
                                  ]
                                },
                                {
                                  "`Module": [
                                    {
                                      "`Alias": [
                                        {
                                          "`Canonical": [
                                            {
                                              "`Alias": [
                                                {
                                                  "`Alias": [
                                                    {
                                                      "`Canonical": [
                                                        {
                                                          "`Alias": [
                                                            {
                                                              "`Alias": [
                                                                {
                                                                  "`Canonical": [
                                                                    {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Canonical": [
                                                                      {
                                                                      "`Alias": [
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      }
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M1__}1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M1"
                                                                      ]
                                                                      }
                                                                      }
                                                                      ]
                                                                      },
                                                                      {
                                                                      "`Hidden": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "{M2__}2"
                                                                      ]
                                                                      }
                                                                      }
                                                                      }
                                                                      ]
                                                                    },
                                                                    {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M2"
                                                                      ]
                                                                      }
                                                                      }
                                                                    }
                                                                  ]
                                                                },
                                                                {
                                                                  "`Identifier": {
                                                                    "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                      },
                                                                      "M2"
                                                                    ]
                                                                  }
                                                                }
                                                              ]
                                                            },
                                                            {
                                                              "`Hidden": {
                                                                "`Identifier": {
                                                                  "`Module": [
                                                                    {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Chain"
                                                                      ]
                                                                    },
                                                                    "{M3__}3"
                                                                  ]
                                                                }
                                                              }
                                                            }
                                                          ]
                                                        },
                                                        {
                                                          "`Resolved": {
                                                            "`Identifier": {
                                                              "`Module": [
                                                                {
                                                                  "`Root": [
                                                                    "None",
                                                                    "Chain"
                                                                  ]
                                                                },
                                                                "M3"
                                                              ]
                                                            }
                                                          }
                                                        }
                                                      ]
                                                    },
                                                    {
                                                      "`Identifier": {
                                                        "`Module": [
                                                          {
                                                            "`Root": [
                                                              "None", "Chain"
                                                            ]
                                                          },
                                                          "M3"
                                                        ]
                                                      }
                                                    }
                                                  ]
                                                },
                                                {
                                                  "`Hidden": {
                                                    "`Identifier": {
                                                      "`Module": [
                                                        {
                                                          "`Root": [
                                                            "None", "Chain"
                                                          ]
                                                        },
                                                        "{M4__}4"
                                                      ]
                                                    }
                                                  }
                                                }
                                              ]
                                            },
                                            {
                                              "`Resolved": {
                                                "`Identifier": {
                                                  "`Module": [
                                                    {
                                                      "`Root": [
                                                        "None", "Chain"
                                                      ]
                                                    },
                                                    "M4"
                                                  ]
                                                }
                                              }
                                            }
                                          ]
                                        },
                                        {
                                          "`Identifier": {
                                            "`Module": [
                                              { "`Root": [ "None", "Chain" ] },
                                              "M4"
                                            ]
                                          }
                                        }
                                      ]
                                    },
                                    "B"
                                  ]
                                }
                              ]
                            }
                          },
                          "None"
                        ]
                      },
                      "canonical": "None",
                      "hidden": "false"
                    }
                  ]
                }
              ],
              "compiled": "false",
              "doc": []
            }
          }
        }
      ]
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ find . -name chain.odocl -size +100000c 
