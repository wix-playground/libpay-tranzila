package com.wix.pay.tranzila

/**
 * @param index              J5 transaction index (returned by Tranzila).
 * @param confirmationCode   J5 transaction confirmation code (returned by Trnazila).
 */
case class TranzilaAuthorization(index: String, confirmationCode: String)
